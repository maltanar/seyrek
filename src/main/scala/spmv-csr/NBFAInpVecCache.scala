package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._
import TidbitsOCM._

// a nonblocking, fully associative cache for serving input vector load reqs.

class NBFAInpVecCache(p: SeyrekParams) extends InpVecLoader(p) {
  val inOrder = false

  // TODO parametrize queue sizes!

  val numReadTxns = 4
  val numCacheTxns = numReadTxns + 2
  val numLines = 4
  val numWordsPerLine = 8
  val numLineBits = numWordsPerLine * p.valWidth
  val numOffsBits = log2Up(numWordsPerLine)
  val numTagBits = p.indWidth - numOffsBits
  val cacheReqID = UInt(width = log2Up(numCacheTxns))
  // this is what gets carried around the cache
  class CacheReq extends Bundle {
    // cache internal ID
    val reqID = UInt(width = log2Up(numCacheTxns))
    // line tag (= line number for a fully associative cache)
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

  class CacheRsp extends Bundle {
    // cache internal ID
    val reqID = UInt(width = log2Up(numCacheTxns))
    // response data
    val data = UInt(width = p.valWidth)
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
  val respQ = Module(new FPGAQueue(cacheRsp, 4)).io

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
  // TODO implement each line as multitap shift register instead?
  val cacheLines = Vec.fill(numLines) {Reg(init = UInt(0, width = numLineBits))}

  // TODO check issued request against cached
  // - if hit, return requested data
  // - if miss, put into waitQ and check loading requests
  //   - if not found in loading requests, issue load and add entry
  //   - if found in loading requests, do nothing
  // alternatively, if we want full/proper miss handling, could put into a
  // separate request queue for

  // TODO issue loads
  // TODO handle load responses -- remove from loading, add to cached, cacheLines

}
