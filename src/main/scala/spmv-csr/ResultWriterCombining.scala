package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsOCM._
import TidbitsStreams._

// combines incoming row results into write bursts
class ResultWriterCombining(p: SeyrekParams) extends Module {
  val io = new RowMajorResultWriterIO(p)

  val entries = 2       // #lines allowed in the ResultWriter
  val burstCount = 8    // burst-sized cachelines
  val bitsPerLine = p.mrp.dataWidth * burstCount
  val elemsPerLine = bitsPerLine / p.valWidth
  val numOffsetBits = log2Up(elemsPerLine)
  val numTagBits = p.indWidth - numOffsetBits
  val numLineBits = log2Up(entries)

  def offs(x: UInt): UInt = {x(numOffsetBits-1, 0)}
  def tag(x: UInt): UInt = {x(numTagBits+numOffsetBits-1, numOffsetBits)}

  class ResultAndTag extends ValIndPair(p.valWidth, p.indWidth) {
    val lineNum = UInt(width = numLineBits)
    override val printfStr = "(v = %d, i = %d, line = %d)\n"
    override val printfElems = {() => Seq(value, ind, lineNum)}
    override def cloneType: this.type = new ResultAndTag().asInstanceOf[this.type]
  }
  val resultAndTag = new ResultAndTag()

  class ComplInfo extends PrintableBundle {
    val lineNum = UInt(width = numLineBits)
    val tag = UInt(width = numTagBits)
    val printfStr = "(line = %d, tag = %d)\n"
    val printfElems = {() => Seq(lineNum, tag)}
    override def cloneType: this.type = new ComplInfo().asInstanceOf[this.type]
  }
  val complInfo = new ComplInfo()

  //==========================================================================

  val sched = Module(new CAM(entries, numTagBits)).io
  val filled = Vec.fill(entries) {Reg(init = UInt(0, numOffsetBits+1))}
  val data = Vec.fill(entries) {Vec.fill(elemsPerLine) {
    Reg(init = UInt(0, width = p.valWidth))
  }}

  val entryMix = Module(new Arbiter(p.vi, 2)).io
  val waitQ = Module(new FPGAQueue(p.vi, 2)).io
  val entryQ = Module(new FPGAQueue(p.vi, 2)).io
  val readyQ = Module(new FPGAQueue(resultAndTag, 2)).io
  val complQ = Module(new FPGAQueue(complInfo, 4)).io
  val bigQ = Module(new FPGAQueue(UInt(width = bitsPerLine), 2)).io

  val regRes = Reg(init = UInt(0, 32))
  when(io.start & io.mode === SeyrekModes.START_INIT) {
    regRes := UInt(0)
  } .elsewhen(io.results.ready & io.results.valid) {
    regRes := regRes + UInt(1)
  }

  val schedEmpty = PopCount(sched.valid_bits) === UInt(0)
  io.finished := regRes === io.csr.rows & schedEmpty

  // ==========================================================================
  // write combiner entry:
  // check incoming element tag in scheduler
  val headTag = tag(entryQ.deq.bits.ind)
  sched.tag := headTag
  sched.write_tag := headTag
  sched.write := Bool(false)

  val hitPos = PriorityEncoder(sched.hits)
  val newPos = sched.freeInd

  val enterAsExisting = sched.hit
  val enterAsNew = !sched.hit & sched.hasFree
  val canEnter = enterAsNew | enterAsExisting

  // entry goes to readyQ if accepted, or to waitQ if not
  entryQ.deq <> DecoupledOutputDemux(canEnter, Seq(waitQ.enq, readyQ.enq))
  readyQ.enq.bits.lineNum := Mux(enterAsNew, newPos, hitPos)
  // add to scheduler when new elements enter the combiner
  when(readyQ.enq.ready & readyQ.enq.valid & enterAsNew) {
    sched.write := Bool(true)
    filled(newPos) := UInt(0) // clear filled status
    data(newPos) := UInt(0)
  }

  // recycle elements that cannot enter the scheduler
  io.results <> entryMix.in(0)
  waitQ.deq <> entryMix.in(1)
  entryMix.out <> entryQ.enq

  // ==========================================================================
  // update data and fill status

  val readyHead = readyQ.deq.bits
  val readyHeadOffs = offs(readyHead.ind)
  val readyHeadTag = tag(readyHead.ind)
  val readyHeadFill = filled(readyHead.lineNum)
  val readyHeadLineData = data(readyHead.lineNum)

  readyQ.deq.ready := complQ.enq.ready
  complQ.enq.valid := Bool(false)
  complQ.enq.bits.lineNum := readyHead.lineNum
  complQ.enq.bits.tag := readyHeadTag

  when(readyQ.deq.ready & readyQ.deq.valid) {
    // set bit in line fill status
    readyHeadFill := readyHeadFill + UInt(1)
    // update line data
    val dataLB = readyHeadOffs * UInt(p.valWidth)
    val dataUB = dataLB + UInt(p.valWidth - 1)
    readyHeadLineData(readyHeadOffs) := readyHead.value
    // when the entire line is filled, signal completion
    when(readyHeadFill === UInt(elemsPerLine-1)) {
      complQ.enq.valid := Bool(true)
    }
  }

  // ==========================================================================
  // when lines are filled, flush their data
  // TODO implement explicit flush mode

  val wrreq = io.memWrReq
  val wrdat = io.memWrDat
  val complLine = complQ.deq.bits.lineNum
  val complTag = complQ.deq.bits.tag

  complQ.deq.ready := bigQ.enq.ready & wrreq.ready
  bigQ.enq.valid := complQ.deq.valid & wrreq.ready
  wrreq.valid := complQ.deq.valid & bigQ.enq.ready
  // pull completed line
  bigQ.enq.bits := data(complLine).toBits
  // use downsizer to emit burst beats
  StreamDownsizer(bigQ.deq, p.mrp.dataWidth) <> io.memWrDat
  // get ready to remove from scheduler and emit write burst request
  sched.clear_tag := complTag
  sched.clear_hit := complQ.deq.ready & complQ.deq.valid
  wrreq.bits.channelID := UInt(0)
  wrreq.bits.isWrite := Bool(true)
  wrreq.bits.addr := io.csr.outVec + complTag * UInt(bitsPerLine/8)
  wrreq.bits.numBytes := UInt(bitsPerLine/8)
  wrreq.bits.metaData := UInt(0)

  // ==========================================================================
  // debug
  val verboseDebug = false
  if(verboseDebug) {
    val queues = Seq(waitQ, entryQ, readyQ, complQ)
    val names = Seq("waiting", "entry", "ready", "compl")
    for((q,n) <- queues zip names) {
      PrintableBundleStreamMonitor(q.enq, Bool(true), "+"+n, true)
      PrintableBundleStreamMonitor(q.deq, Bool(true), "-"+n, true)
    }

    when(bigQ.enq.valid & bigQ.enq.ready) {
      printf("line data = ")
      for(i <- 0 until burstCount) {
        printf("%d = %d, ", UInt(i), data(complLine)(i))
      }
      printf("\n")
    }
  }
}
