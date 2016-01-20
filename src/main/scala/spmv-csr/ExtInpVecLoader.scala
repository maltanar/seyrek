package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._
import TidbitsOCM._

// services inp.vec. load requests from DRAM (or other high latency medium)
// with multiple outstanding requests and support for out-of-order returns

class ExtInpVecLoader(p: SeyrekParams, chanIDBase: Int) extends InpVecLoader(p) {
  val inOrder: Boolean = false
  val maxTxns: Int = p.chanConfig("inpvec").maxReadTxns

  // pool of available read request IDs
  val readReqPool = Module(
    new ReqIDQueue(p.mrp.idWidth, maxTxns, chanIDBase)).io
  readReqPool.doInit := (io.mode === SeyrekModes.START_CONFIG) & io.start
  readReqPool.initCount := io.contextReqCnt

  // data associated with read req waits here for the response to come
  val readWait = Mem(io.loadReq.bits, maxTxns)

  val readyReqs = StreamJoin(
    inA = io.loadReq, inB = readReqPool.idOut,
    genO = UInt(width = p.indWidth), join =
    {(rq: ValIndInd, id: UInt) => rq.j} // loads generated from j
  )

  // when a read transaction happens, preserve the read request data in
  // the readWait memory. this mem is indexed by the request ID, so the
  // memory line corresponding to an accepted txn's ID is always free
  when( readyReqs.ready & readyReqs.valid) {
    // note how the id is "rebased" (p.chanID may start from a nonzero val.)
    readWait(readReqPool.idOut.bits - UInt(chanIDBase)) := io.loadReq.bits
  }

  // convert indices to read requests
  // TODO ReadArray should also take in id stream, coming from the id pool
  // as a direct signal for now. safe because StreamJoin synchronizes
  // both the request and id streams.
  val rr = ReadArray(readyReqs, io.contextBase, readReqPool.idOut.bits, p.mrp)
  rr  <> io.mainMem.memRdReq

  // fork read responses into read data and channel ID
  // the read data is returned from the ContextMem, while the channel ID
  // will be recycled into the free request ID pool
  val readRspFork = Module(new StreamFork(
    genIn = io.mainMem.memRdRsp.bits, genA = readReqPool.idIn.bits,
    genB = io.mainMem.memRdRsp.bits.readData,
    forkA = {x: GenericMemoryResponse => x.channelID},
    forkB = {x: GenericMemoryResponse => x.readData}
  )).io

  io.mainMem.memRdRsp <> readRspFork.in
  readRspFork.outA <> readReqPool.idIn // recycle ID back to pool

  readRspFork.outB.ready := io.loadRsp.ready
  io.loadRsp.valid := readRspFork.outB.valid
  io.loadRsp.bits.vectorVal := readRspFork.outB.bits
  // load the corresponding read req context from the readWait mem
  // TODO don't make the context read combinational, add an extra stage here?
  val restoredReadCtx = readWait(readRspFork.outA.bits - UInt(chanIDBase))
  io.loadRsp.bits.matrixVal := restoredReadCtx.v
  io.loadRsp.bits.rowInd := restoredReadCtx.i


  // printfs for debug
  /*
  when( readyReqs.ready & readyReqs.valid) {
    printf("ExtInpVec load req. val = %d i = %d j = %d \n",
      io.loadReq.bits.v, io.loadReq.bits.i, io.loadReq.bits.j
    )
  }

  when( io.loadRsp.ready & io.loadRsp.valid) {
    printf("ExtInpVec load rsp. val1 = %d val2 = %d i = %d \n",
      io.loadRsp.bits.matrixVal, io.loadRsp.bits.vectorVal,
      io.loadRsp.bits.rowInd
    )
  }
  */
}
