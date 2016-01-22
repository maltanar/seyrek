package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._
import TidbitsOCM._

// a nonblocking, fully associative cache for serving input vector load reqs.

class NBFAInpVecCache(p: SeyrekParams) extends InpVecLoader(p) {
  val inOrder = false

  // TODO parametrize

  if(p.valWidth > p.mrp.dataWidth || p.mrp.dataWidth % p.valWidth != 0)
    throw new Exception("Unsupported valWidth:dataWidth ratio")

  val numReadTxns = 4
  val numCacheTxns = numReadTxns + 2
  val numLines = 4

  val burstCount = 8
  val bytesPerMemWord = (p.mrp.dataWidth / 8)
  val bytesPerLine = bytesPerMemWord * burstCount
  val bytesPerElem = p.valWidth / 8
  val elemsPerLine = bytesPerLine / bytesPerElem
  val elemsPerMemWord = bytesPerMemWord / bytesPerElem

  val numOffsBits = log2Up(elemsPerLine)
  val numTagBits = p.indWidth - numOffsBits
  val cacheReqID = UInt(width = log2Up(numCacheTxns))
  // this is what gets carried around the cache
  class CacheReq extends Bundle {
    // cache internal ID
    val reqID = UInt(width = log2Up(numCacheTxns))
    // line tag
    val tag = UInt(width = numTagBits)
    // word offset in line
    val offs = UInt(width = numOffsBits)

    override def cloneType: this.type = new CacheReq().asInstanceOf[this.type]
  }
  val cacheReq = new CacheReq() // useful as cloneType
  // turn the external (Aij, i, j) into a cache request
  def makeCacheReq(rq: ValIndInd, rid: UInt): CacheReq = {
    val cr = new CacheReq()
    cr.reqID := rid
    cr.tag := rq.j(p.indWidth-1, numOffsBits)
    cr.offs := rq.j(numOffsBits-1, 0)
    cr
  }

  class CacheTagRsp extends Bundle {
    // cache internal ID
    val reqID = UInt(width = log2Up(numCacheTxns))
    // word offset in line
    val offs = UInt(width = numOffsBits)
    // location of line
    val lineNum = UInt(width = log2Up(numLines))
    override def cloneType: this.type = new CacheTagRsp().asInstanceOf[this.type]
  }
  val cacheTagRsp = new CacheTagRsp()

  class CacheRsp extends Bundle {
    // cache internal ID
    val reqID = UInt(width = log2Up(numCacheTxns))
    // response data
    val data = UInt(width = p.valWidth)

    override def cloneType: this.type = new CacheRsp().asInstanceOf[this.type]
  }
  val cacheRsp = new CacheRsp()

  // ==========================================================================

  // cloakroom -- don't carry around the entire request
  val cloakroom = Mem(p.vii, numCacheTxns)
  val cacheReqIDPool = Module(
    new ReqIDQueue(log2Up(numCacheTxns), numCacheTxns, 0)).io

  val readyReqs = StreamJoin(inA = io.loadReq, inB = cacheReqIDPool.idOut,
    genO = cacheReq, join = makeCacheReq
  )

  // add to cloakroom when request arrives
  when( readyReqs.ready & readyReqs.valid) {
    cloakroom(readyReqs.bits.reqID) := io.loadReq.bits
  }

  // requets ready to be processed
  val reqQ = Module(new FPGAQueue(cacheReq, 4)).io
  readyReqs <> reqQ.enq

  // responses ready to be processed
  val respQCapacity = 4
  val respQ = Module(new FPGAQueue(cacheRsp, respQCapacity)).io

  val readyResps = Module(new StreamFork(
    genIn = cacheRsp, genA = cacheReqID, genB = p.wu,
    forkA = {a: CacheRsp => a.reqID},
    forkB = {a: CacheRsp =>
      val newWU = new WorkUnit(p.valWidth, p.indWidth)
      newWU.vectorVal := a.data
      // other WU elements will be fetched from the cloakroom
      newWU
    }
  )).io

  respQ.deq <> readyResps.in
  readyResps.outA <> cacheReqIDPool.idIn
  readyResps.outB <> io.loadRsp

  // retrieve rowInd and matrixVal upon exit
  io.loadRsp.bits.rowInd := cloakroom(readyResps.in.bits.reqID).i
  io.loadRsp.bits.matrixVal := cloakroom(readyResps.in.bits.reqID).v


  // "lazy dataflow" shuffler for the requests -- TODO

  // CAM schedulers for keeping the current cache status
  val cached = Module(new CAM(numLines, numTagBits)).io
  val loading = Module(new CAM(numReadTxns, numTagBits)).io

  // data storage
  val memLatency = 1
  // TODO set to a large number to ensure BRAM gen -- real # words probably
  // too shallow to be mapped into a BRAM
  val mem = Module(new DualPortBRAM(1024, bytesPerMemWord*8)).io
  // for reading cachelines to respond to requests
  val readPort = mem.ports(0)
  // for writing retrieved cachelines into memory
  val writePort = mem.ports(1)
  // disable write enables by default
  readPort.req.writeEn := Bool(false)
  writePort.req.writeEn := Bool(false)

  // TODO check issued request against cached
  // - if hit, return requested data
  // - if miss, put into waitQ and check loading requests
  //   - if not found in loading requests, issue load and add entry
  //   - if found in loading requests, do nothing

  cached.tag := reqQ.deq.bits.tag

  val hitQ = Module(new FPGAQueue(cacheTagRsp, 2)).io

  hitQ.enq.bits.lineNum := PriorityEncoder(cached.hits)
  hitQ.enq.bits.reqID := reqQ.deq.bits.reqID
  hitQ.enq.bits.offs := reqQ.deq.bits.offs
  hitQ.enq.valid := reqQ.deq.valid & cached.hit

  // TODO handshake over latency to fetch data and put into respQ
  // hitQ -> [mem] -> respQ
  val mAddrBase = hitQ.deq.bits.lineNum * UInt(burstCount)
  val mAddrOffs = hitQ.deq.bits.offs / UInt(elemsPerMemWord)
  readPort.req.addr := mAddrBase + mAddrOffs

  val canResp = respQ.count < UInt(respQCapacity - memLatency)

  hitQ.deq.ready := canResp
  respQ.enq.valid := ShiftRegister(canResp & hitQ.deq.valid, memLatency)
  respQ.enq.bits.reqID := ShiftRegister(hitQ.deq.bits.reqID, memLatency)

  // if there are multiple inp.vec elements per word, need to choose subword
  if(elemsPerMemWord > 1) {
    val mSubWord = hitQ.deq.bits.offs & UInt(elemsPerMemWord - 1)
    val subWord = ShiftRegister(mSubWord, memLatency)
    val wordStart = subWord * UInt(p.valWidth)
    val wordEnd = ((subWord + UInt(1)) * UInt(p.valWidth)) - UInt(1)
    respQ.enq.bits.data := readPort.rsp.readData(wordEnd, wordStart)
  } else {
    respQ.enq.bits.data := readPort.rsp.readData
  }

  // TODO issue loads
  // TODO handle load responses -- remove from loading, add to cached, cacheLines

}

class StreamShufflerIO[T <: Data](gen: T) extends Bundle {
  val in = Decoupled(gen).flip
  val out = Decoupled(gen)

  override def cloneType: this.type = new StreamShufflerIO(gen).asInstanceOf[this.type]
}

// TODO the StreamShuffler system will need some experimentation to ensure it
// does what we want, prioritizing old (waitQ) values but not too much
class StreamShuffler[T <: Data](gen: T, numOut: Int, numWait: Int) extends Module {
  val io = new StreamShufflerIO(gen)

  val outQ = Module(new FPGAQueue(gen, numOut)).io
  val waitQ = Module(new FPGAQueue(gen, numWait)).io

  val mix = Module(new Arbiter(gen, 2)).io
  waitQ.deq <> mix.in(0)
  io.in <> mix.in(1)
  mix.out <> waitQ.enq

  // expose data in outQ to both waitQ and output
  io.out.bits := outQ.deq.bits
  waitQ.enq.bits := outQ.deq.bits

  // first priority is the output; put outQ contents there if we can
  io.out.valid := outQ.deq.valid
  // if io.out is not ready, try to go to the waitQ
  waitQ.enq.valid := outQ.deq.valid & !io.out.ready
}
