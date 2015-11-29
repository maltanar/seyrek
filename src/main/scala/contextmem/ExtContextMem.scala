package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._
import TidbitsOCM._

class ExtContextMemParams(
  val readTxns: Int,
  val writeTxns: Int,
  idBits: Int,
  dataBits: Int,
  chanID: Int,
  mrp: MemReqParams
) extends ContextMemParams(idBits, dataBits, chanID, mrp)



// TODO add support for non-word accesses

class OoOExtContextMem(p: ExtContextMemParams) extends ContextMem(p) {
  val inOrder: Boolean = false

  // important to distinguish between two types of ID data here:
  // - the request IDs that get sent to memory system (width=p.mrp.idWidth)
  // - the context IDs (row indices) in Seyrek (width=p.idBits)

  // pool of available read request IDs
  val readReqPool = Module(
    new ReqIDQueue(p.mrp.idWidth, p.readTxns, p.chanID)).io

  // data associated with read req waits here for the response to come
  val readWait = Mem(io.contextLoadReq.bits, p.readTxns)

  val readyReqs = StreamJoin(
    inA = io.contextLoadReq, inB = readReqPool.idOut,
    genO = UInt(width = p.idBits), join =
    {(rq: ValIndPair, id: UInt) => rq.ind}
  )

  // when a read transaction happens, preserve the read request data in
  // the readWait memory. this mem is indexed by the request ID, so the
  // memory line corresponding to an accepted txn's ID is always free
  when( readyReqs.ready & readyReqs.valid) {
    // note how the id is "rebased" (p.chanID may start from a nonzero val.)
    readWait(readReqPool.idOut.bits - UInt(p.chanID)) := io.contextLoadReq.bits
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

  readRspFork.outB.ready := io.contextLoadRsp.ready
  io.contextLoadRsp.valid := readRspFork.outB.valid
  io.contextLoadRsp.bits.matrixVal := readRspFork.outB.bits
  // load the corresponding read req context from the readWait mem
  // TODO don't make the context read combinational, add an extra stage here
  val restoredReadCtx = readWait(readRspFork.outA.bits - UInt(p.chanID))
  io.contextLoadRsp.bits.vectorVal := restoredReadCtx.value
  io.contextLoadRsp.bits.rowInd := restoredReadCtx.ind

  // =======================================================================
  // context writes, pretty similar to how OoO reads are handled
  // =======================================================================

  // pool of available write request IDs
  val writeReqPool = Module(
    new ReqIDQueue(p.mrp.idWidth, p.writeTxns, p.chanID)).io

  // data associated with write req waits here for the response to come
  val writeWait = Mem(io.contextSaveReq.bits.ind, p.writeTxns)
  // sync request ID stream and write request stream
  val readyWriteReqs = StreamJoin(
    inA = io.contextSaveReq, inB = writeReqPool.idOut,
    genO = io.contextSaveReq.bits, join =
    {(rq: ValIndPair, id: UInt) => rq}
  )
  // when a write transaction happens, preserve the write request data (index)
  // in the writeWait memory
  when( readyWriteReqs.ready & readyWriteReqs.valid) {
    // note how the id is "rebased" (p.chanID may start from a nonzero val.)
    writeWait(writeReqPool.idOut.bits - UInt(p.chanID)) := io.contextSaveReq.bits.ind
  }

  // fork write data indices and writes as two streams
  val saveMemFork = Module(new StreamFork(
    genIn = readyWriteReqs.bits, genA = UInt(width = p.idBits),
    genB = UInt(width = p.dataBits),
    forkA = {x: ValIndPair => x.ind},
    forkB = {x: ValIndPair => x.value}
  )).io
  readyWriteReqs <> saveMemFork.in
  // convert indices to write stream
  // TODO WriteArray should also take in id stream, coming from the id pool
  // as a direct signal for now. safe because StreamJoin synchronizes
  // both the request and id streams.
  val wr = WriteArray(saveMemFork.outA, io.contextBase, writeReqPool.idOut.bits, p.mrp)
  wr <> io.mainMem.memWrReq
  saveMemFork.outB <> io.mainMem.memWrDat

  // handling write responses:
  // fork write responses into channel ID + context saved ID
  val writeRspFork = Module(new StreamFork(
    genIn = io.mainMem.memWrRsp.bits, genA = writeReqPool.idIn.bits,
    genB = io.contextSaveRsp.bits,
    forkA = {x: GenericMemoryResponse => x.channelID},
    forkB = {x: GenericMemoryResponse => UInt(0, width = p.idBits)} // bogus 0
  )).io
  io.mainMem.memWrRsp <> writeRspFork.in
  writeRspFork.outA <> writeReqPool.idIn // recycle ID back into pool
  writeRspFork.outB <> io.contextSaveRsp // we'll override the .bits here

  // load the corresponding write req context from the writeWait mem
  // TODO don't make the context read combinational, add an extra stage here
  val restoredWrCtx = writeWait(io.mainMem.memWrRsp.bits.channelID - UInt(p.chanID))
  io.contextSaveRsp.bits := restoredWrCtx // saved ID from restoredWrCtx

  // finished signal for the ExtContextMem
  io.finished := Bool(true)
}

class ExtContextMem(p: ExtContextMemParams) extends ContextMem(p) {
  val inOrder: Boolean = (p.readTxns == 1 && p.writeTxns == 1)

  // ExtContextMem does not need flush or init
  val flushOrInit =
    (io.mode === SeyrekModes.START_INIT || io.mode === SeyrekModes.START_FLUSH)
  io.finished := Reg(init=Bool(false), next=io.start & flushOrInit)

  if(inOrder) {
    // context load handling
    // val-ind pairs wait in a queue for the load responses to arrive
    val waitLoad = Module(new FPGAQueue(io.contextLoadReq.bits, p.readTxns+1)).io
    // fork the request stream into the queue and extmem load reqs
    val loadFork = Module(new StreamFork(
      genIn = io.contextLoadReq.bits, genA = io.contextLoadReq.bits,
      genB = UInt(width = p.idBits), forkA = {x: ValIndPair => x},
      forkB = {x: ValIndPair => x.ind}
    )).io

    io.contextLoadReq <> loadFork.in
    loadFork.outA <> waitLoad.enq
    // convert indices to read requests
    val rr = ReadArray(loadFork.outB, io.contextBase, UInt(p.chanID), p.mrp)
    rr  <> io.mainMem.memRdReq

    // join the waiting val-ind pair with the loaded value on response
    // and drive the load responses with this stream
    StreamJoin(
      inA = waitLoad.deq, inB = io.mainMem.memRdRsp, genO = io.contextLoadRsp.bits,
      join = {(vi: ValIndPair, m: GenericMemoryResponse) =>
        WorkUnit(m.readData, vi.value, vi.ind)}
    ) <> io.contextLoadRsp

    // context save handling
    val waitSave = Module(new FPGAQueue(io.contextSaveRsp.bits, p.writeTxns+1)).io
    val saveFork = Module(new StreamFork(
      genIn = io.contextSaveReq.bits, genA = io.contextSaveReq.bits,
      genB = io.contextSaveRsp.bits, forkA = {x: ValIndPair => x},
      forkB = {x: ValIndPair => x.ind}
    )).io
    val saveMemFork = Module(new StreamFork(
      genIn = io.contextSaveReq.bits, genA = UInt(width = p.idBits),
      genB = UInt(width = p.dataBits),
      forkA = {x: ValIndPair => x.ind},
      forkB = {x: ValIndPair => x.value}
    )).io

    io.contextSaveReq <> saveFork.in
    saveFork.outA <> saveMemFork.in
    saveFork.outB <> waitSave.enq
    val wr = WriteArray(saveMemFork.outA, io.contextBase, UInt(p.chanID), p.mrp)
    wr <> io.mainMem.memWrReq
    saveMemFork.outB <> io.mainMem.memWrDat

    // sync on extmem write responses before issuing save response
    StreamJoin(
      inA = waitSave.deq, inB = io.mainMem.memWrRsp, genO = io.contextSaveRsp.bits,
      join = {(i: UInt, mr: GenericMemoryResponse) => i}
    ) <> io.contextSaveRsp

  } else throw new Exception("OoO ExtContextMem not yet supported")
}
