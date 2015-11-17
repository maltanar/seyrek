package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._

class ExtContextMemParams(
  val readTxns: Int,
  val writeTxns: Int,
  idBits: Int,
  dataBits: Int,
  chanID: Int,
  mrp: MemReqParams
) extends ContextMemParams(idBits, dataBits, chanID, mrp)

// TODO add support for multiple outstanding requests
// TODO inst. reorder buffer depending on mem sys OoO-ness
// TODO add support for non-word accesses

class ExtContextMem(p: ExtContextMemParams) extends ContextMem(p) {
  val inOrder: Boolean = (p.readTxns == 1 && p.writeTxns == 1)

  // ExtContextMem does not need flush or init
  val flushOrInit =
    (io.mode === SeyrekModes.START_INIT || io.mode === SeyrekModes.START_FLUSH)
  io.finished := Reg(init=Bool(false), next=io.start & flushOrInit)

  if(inOrder) {
    // context load handling
    // val-ind pairs wait in a queue for the load responses to arrive
    val waitLoad = Module(new Queue(io.contextLoadReq.bits, p.readTxns+1)).io
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
    val waitSave = Module(new Queue(io.contextSaveRsp.bits, p.writeTxns+1)).io
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
