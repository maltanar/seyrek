package Seyrek

import Chisel._
import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.ocm._

class ShufflingInpVecCache(p: SeyrekParams, chanIDBase: Int) extends InpVecLoader(p) {
  val inOrder = false
  val verboseDebug = true

  // TODO parametrize

  if(p.valWidth > p.mrp.dataWidth || p.mrp.dataWidth % p.valWidth != 0)
    throw new Exception("Unsupported valWidth:dataWidth ratio")

  val numReadTxns = 8
  val numCacheTxns = 64
  val numLines = 1024

  if(!isPow2(numLines))
    throw new Exception("Cache lines must be power of two")

  val burstCount = 8
  val bytesPerMemWord = (p.mrp.dataWidth / 8)
  val bytesPerLine = bytesPerMemWord * burstCount
  val bytesPerElem = p.valWidth / 8
  val elemsPerLine = bytesPerLine / bytesPerElem
  val elemsPerMemWord = bytesPerMemWord / bytesPerElem
  val bitsPerLine = bytesPerLine * 8

  val numOffsBits = log2Up(elemsPerLine)
  val numIndBits = log2Up(numLines)
  val numTagBits = p.indWidth - (numOffsBits + numIndBits)

  // cache internal types
  class CacheReq extends CloakroomBundle(numCacheTxns) {
    // line number
    val lineNum = UInt(width = numIndBits)
    // line tag
    val tag = UInt(width = numTagBits)
    // word offset in line
    val offs = UInt(width = numOffsBits)

    override val printfStr = "id: %d lineNum: %d tag: %d offs: %d\n"
    override val printfElems = {() => Seq(id, lineNum, tag, offs)}

    override def cloneType: this.type = new CacheReq().asInstanceOf[this.type]
  }

  class CacheTagRsp extends CacheReq {
    // hit or miss?
    val isHit = Bool()
    // load in progress?
    val isLoadInProgress = Bool()
    // response data if hit
    val data = UInt(width = p.valWidth)

    override val printfStr = "id: %d hit: %d inProg: %d data: %d \n"
    override val printfElems = {() => Seq(id, isHit, isLoadInProgress, data)}

    override def cloneType: this.type = new CacheTagRsp().asInstanceOf[this.type]
  }

  // cloneTypes for internal types
  val cacheReq = new CacheReq()
  val cacheTagRsp = new CacheTagRsp()

  // ==========================================================================
  // structures for keeping the cache state
  // valid - inProgress - tag
  val tagStore = Module(new DualPortBRAM(numIndBits, 2 + numTagBits)).io
  val tagLat = 1
  val tagRead = tagStore.ports(0)
  val tagWrite = tagStore.ports(1)

  tagRead.req.writeEn := Bool(false)
  tagRead.req.writeData := UInt(0)

  // tag init logic
  val startInit = (io.mode === SeyrekModes.START_INIT & io.start)
  val regInitActive = Reg(next = startInit)
  val regTagInitAddr = Reg(init = UInt(0, 1+numIndBits))

  io.finished := startInit & (regTagInitAddr === UInt(numLines))

  when(regInitActive) {
    when(!startInit) { regTagInitAddr := UInt(0) }
    .elsewhen(regTagInitAddr < UInt(numLines)) {
      regTagInitAddr := regTagInitAddr + UInt(1)
      tagRead.req.addr := regTagInitAddr
      tagRead.req.writeEn := Bool(true)
    }
  }

  // cacheline data
  val datStore = Module(new DualPortBRAM(numIndBits+numOffsBits, p.mrp.dataWidth)).io
  val datLat = 1
  val datRead = datStore.ports(0)
  val datWrite = datStore.ports(1)

  datRead.req.writeEn := Bool(false)

  val respQ = Module(new FPGAQueue(cacheTagRsp, 2)).io
  val missQ = Module(new FPGAQueue(cacheReq, numCacheTxns)).io
  val newReqQ = Module(new FPGAQueue(cacheReq, 2)).io
  val reqQ = Module(new FPGAQueue(cacheReq, 2)).io

  // TODO tagRespQ size should be max(tagLat, datLat) + 2
  if(tagLat != datLat)
    throw new Exception("tag latency != data latency, needs fix")
  val tagRespQ = Module(new FPGAQueue(cacheTagRsp, tagLat + 2)).io

  // mix in resolved requests into the request stream
  // IMPROVE use own cache port for resolving requests?
  val reqMix = Module(new Arbiter(cacheReq, 2)).io
  newReqQ.deq <> reqMix.in(0)
  missQ.deq <> reqMix.in(1)

  reqMix.out <> reqQ.enq

  // CAM for tracking in-flight loads
  val pendingLoads = Module(new CAM(numReadTxns, numTagBits)).io

  // ==========================================================================
  // cloakroom -- don't carry around the entire request

  // turn the external (Aij, i, j, rl) into a cache request
  def viilToCacheReq(rq: ValIndIndLen): CacheReq = {
    val cr = new CacheReq()
    cr.tag := rq.j(p.indWidth-1, numOffsBits + numIndBits)
    cr.lineNum := rq.j(numOffsBits + numIndBits - 1, numOffsBits)
    cr.offs := rq.j(numOffsBits-1, 0)
    cr
  }

  def makeWU(origRq: ValIndIndLen, rsp: CacheTagRsp): WorkUnit = {
    val wu = new WorkUnitAndLen(p.valWidth, p.indWidth)
    wu.matrixVal := origRq.v
    wu.vectorVal := rsp.data
    wu.rowInd := origRq.i
    wu.rowLen := origRq.rl
    wu
  }

  val cloakroom = Module(new CloakroomLUTRAM(
    num = numCacheTxns, genA = io.loadReq.bits, genC = cacheTagRsp,
    undress = viilToCacheReq, dress = makeWU
  )).io

  io.loadReq <> cloakroom.extIn
  cloakroom.intOut <> newReqQ.enq

  respQ.deq <> cloakroom.intIn
  cloakroom.extOut <> io.loadRsp

  // ==========================================================================
  // tag lookup logic

  // direct-mapped tag+data lookup with handshaking over latency
  // tag memory addrs have 1:1 correspondence with lines
  tagRead.req.addr := reqQ.deq.bits.lineNum
  // ...but a line spans several elements of the data memory
  val mAddrBase = reqQ.deq.bits.lineNum * UInt(burstCount)
  val mAddrOffs = reqQ.deq.bits.offs / UInt(elemsPerMemWord)
  datRead.req.addr := mAddrBase + mAddrOffs

  val tagHazard = /*(reqQ.deq.bits.lineNum === tagWrite.req.addr) & */tagWrite.req.writeEn
  val canReadTag = (tagRespQ.count < UInt(2)) & !tagHazard

  val origReq = ShiftRegister(reqQ.deq.bits, tagLat)

  val tagMatch = tagRead.rsp.readData(numTagBits-1, 0) === origReq.tag
  val inProg = tagRead.rsp.readData(numTagBits)
  val tagValid = tagRead.rsp.readData(numTagBits + 1)
  val tagHit = tagMatch & tagValid

  reqQ.deq.ready := canReadTag

  // set up connections to the tagRespQ enq.
  tagRespQ.enq.valid := ShiftRegister(canReadTag & reqQ.deq.valid, tagLat)
  origReq <> tagRespQ.enq.bits
  tagRespQ.enq.bits.isHit := tagHit
  tagRespQ.enq.bits.isLoadInProgress := inProg

  // data read is slightly trickier
  // if there are multiple inp.vec elements per word, need to choose subword
  if(elemsPerMemWord > 1) {
    val subWord = origReq.offs & UInt(elemsPerMemWord - 1)
    val wordStart = subWord * UInt(p.valWidth)
    val wordEnd = ((subWord + UInt(1)) * UInt(p.valWidth)) - UInt(1)
    tagRespQ.enq.bits.data := datRead.rsp.readData(wordEnd, wordStart)
  } else {
    tagRespQ.enq.bits.data := datRead.rsp.readData
  }

  // route tag lookup output into appropriate queue
  val tagRespDest = Module(new DecoupledOutputDemux(cacheTagRsp, 2)).io
  tagRespQ.deq <> tagRespDest.in
  tagRespDest.out(0) <> missQ.enq
  tagRespDest.out(1) <> respQ.enq

  tagRespDest.sel := tagRespQ.deq.bits.isHit

  // =========================================================================
  // miss handling: issue load requests
  val missHead = missQ.enq
  // shorthands for accessing main memory
  val mreqQ = Module(new FPGAQueue(new GenericMemoryRequest(p.mrp), 2)).io
  val mrspQ = Module(new FPGAQueue(new GenericMemoryResponse(p.mrp), 2)).io
  mreqQ.deq <> io.mainMem.memRdReq
  io.mainMem.memRdRsp <> mrspQ.enq
  val mreq = mreqQ.enq
  val mrsp = mrspQ.deq
  // check missQ enqs against pending loads, prepare to write
  pendingLoads.tag := missHead.bits.tag
  pendingLoads.write_tag := missHead.bits.tag
  pendingLoads.write := Bool(false)
  val alreadyLoading = pendingLoads.hit | tagRespQ.deq.bits.isLoadInProgress
  val canHandleMiss = pendingLoads.hasFree & mreq.ready
  val freePos = pendingLoads.freeInd
  val loadingLine = Vec.fill(numReadTxns) {Reg(init = UInt(0, numIndBits))}
  val loadingTag = Vec.fill(numReadTxns) {Reg(init = UInt(0, numTagBits))}
  val loadingPrg = Vec.fill(numReadTxns) {Reg(init = UInt(0, 4))}

  // prepare for issuing mem req for the missed cacheline
  mreq.valid := Bool(false)
  mreq.bits.channelID := UInt(chanIDBase) + freePos
  mreq.bits.isWrite := Bool(false)
  val theLine = Cat(missHead.bits.tag, missHead.bits.lineNum)
  mreq.bits.addr := io.contextBase + theLine * UInt(bytesPerLine)
  mreq.bits.numBytes := UInt(bytesPerLine)
  mreq.bits.metaData := UInt(0)

  when(missHead.valid & missHead.ready) {
    when(!alreadyLoading & canHandleMiss) {
      // add to pending loads and book info
      pendingLoads.write := Bool(true)
      loadingLine(freePos) := missHead.bits.lineNum
      loadingTag(freePos) := missHead.bits.tag
      loadingPrg(freePos) := UInt(0)
      // issue memory request
      mreq.valid := Bool(true)
      if(verboseDebug) {
        printf("New load: id = %d line = %d tag = %d \n",
          freePos, missHead.bits.lineNum, missHead.bits.tag
        )
      }
    }
  }

  // =========================================================================
  // handle read responses from main memory
  val mrspID = mrsp.bits.channelID - UInt(chanIDBase)
  val rspsReceived = loadingPrg(mrspID)
  val targetLine = loadingLine(mrspID)

  mrsp.ready := Bool(true)
  // set up tag write
  tagWrite.req.writeEn := Bool(false)
  tagWrite.req.addr := targetLine
  tagWrite.req.writeData := Cat(Bool(false), Bool(true), loadingTag(mrspID))
  // set up data write
  datWrite.req.writeEn := Bool(false)
  val wAddrBase = targetLine * UInt(burstCount)
  val wAddrOffs = rspsReceived
  datWrite.req.addr := wAddrBase + wAddrOffs
  datWrite.req.writeData := mrsp.bits.readData
  // prepare to remove from pending loads
  pendingLoads.clear_tag := loadingTag(mrspID)
  pendingLoads.clear_hit := Bool(false)

  when(mrsp.ready & mrsp.valid) {
    // activate data write
    datWrite.req.writeEn := Bool(true)
    // increment progress counter
    rspsReceived := rspsReceived + UInt(1)
    when(rspsReceived === UInt(0)) {
      // write valid = 0 when first response received
      tagWrite.req.writeEn := Bool(true)
    } .elsewhen(rspsReceived === UInt(burstCount-1)) {
      // last burst beat received -- data is now valid
      tagWrite.req.writeEn := Bool(true)
      tagWrite.req.writeData := Cat(Bool(true), Bool(false), loadingTag(mrspID))
      // remove from scheduler
      pendingLoads.clear_hit := Bool(true)
      if(verboseDebug) {
        printf("Completed load: id = %d line = %d tag = %d \n",
          mrspID, targetLine, loadingTag(mrspID)
        )
      }
    }
  }

  // ==========================================================================
  // debug
  if(verboseDebug) {
    val queues = Seq(reqQ, tagRespQ, missQ, respQ)
    val names = Seq("req", "tagResp", "miss", "resp")
    for((q,n) <- queues zip names) {
      PrintableBundleStreamMonitor(q.enq, Bool(true), "+"+n, true)
      //PrintableBundleStreamMonitor(q.deq, Bool(true), "-"+n, true)
    }
  }
}
