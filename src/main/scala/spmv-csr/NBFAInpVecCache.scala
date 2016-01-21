package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._
import TidbitsOCM._

class NBFAInpVecCache(p: SeyrekParams) extends InpVecLoader(p) {
  val inOrder = false

  val numReadTxns = 4
  val numCacheTxns = numReadTxns + 2
  val numLines = 4
  val numWordsPerLine = 8
  val numLineBits = numWordsPerLine * p.valWidth
  val numOffsBits = log2Up(numWordsPerLine)
  val numTagBits = p.indWidth - numOffsBits


  // cloakroom -- don't carry around the entire request
  val cloakroom = Mem(p.vi, numCacheTxns)
  val cacheReqIDPool = Module(
    new ReqIDQueue(log2Up(numCacheTxns), numCacheTxns, 0)).io

  val readyReqs = StreamJoin(inA = io.loadReq, inB = cacheReqIDPool.idOut,
    genO = p.i, join = {(rq: ValIndInd, id: UInt) => rq.j}
  )

  // add to cloakroom when request arrives
  when( readyReqs.ready & readyReqs.valid) {
    // note how the id is "rebased" (p.chanID may start from a nonzero val.)
    cloakroom(cacheReqIDPool.idOut.bits) := io.loadReq.bits
  }

  // TODO remove from cloakroom and recycle req id upon exit

  // "lazy dataflow" shuffler
  // TODO parametrize queue sizes
  val waitQ = Module(new FPGAQueue(p.i, 4)).io
  val reqQ = Module(new FPGAQueue(p.i, 4)).io
  val reqMix = Module(new RRArbiter(p.i, 2)).io
  io.loadReq <> reqMix.in(0)
  waitQ.deq <> reqMix.in(1)
  reqMix.out <> reqQ.enq

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
