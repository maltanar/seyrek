package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._

class ExtContextMemParams(
  val readTxns: Int,
  val writeTxns: Int,
  val readIDBase: Int,
  val writeIDBase: Int,
  idBits: Int,
  dataBits: Int,
  mrp: MemReqParams
) extends ContextMemParams(idBits, dataBits, mrp)

// TODO add support for multiple outstanding requests
// TODO inst. reorder buffer depending on mem sys OoO-ness
// TODO add support for non-word accesses

class ExtContextMem(p: ExtContextMemParams) extends ContextMem(p) {
  val inOrder: Boolean = (p.readTxns == 1 && p.writeTxns == 1)

  // generate a comb. circuit for turning val-ind pairs' inds into
  // memory requests for accessing from the proper context mem addr
  def makeReadReq(r: ValIndPair, id: UInt): GenericMemoryRequest = {
    val rreq = GenericMemoryRequest(p.mrp)
    rreq.channelID := id
    rreq.isWrite := Bool(false)
    rreq.addr := io.contextBase + r.ind * UInt(p.dataBits/8)
    rreq.numBytes := UInt(p.dataBits/8)
    rreq.metaData := UInt(0)
    return rreq
  }

  def makeWriteReq(w: ValIndPair, id: UInt): GenericMemoryRequest = {
    val wreq = GenericMemoryRequest(p.mrp)
    wreq.channelID := id
    wreq.isWrite := Bool(true)
    wreq.addr := io.contextBase + w.ind * UInt(p.dataBits/8)
    wreq.numBytes := UInt(p.dataBits/8)
    wreq.metaData := UInt(0)
    return wreq
  }

  if(inOrder) {
    // context load handling
    // val-ind pairs wait in a queue for the load responses to arrive
    val waitLoad = Module(new Queue(io.contextLoadReq.bits, p.readTxns+1)).io
    // fork the request stream into the queue and extmem load reqs
    val loadFork = Module(new StreamFork(
      genIn = io.contextLoadReq.bits, genA = io.contextLoadReq.bits,
      genB = GenericMemoryRequest(p.mrp), forkA = {x: ValIndPair => x},
      forkB = {x: ValIndPair => makeReadReq(x, UInt(p.readIDBase))}
    )).io

    io.contextLoadReq <> loadFork.in
    loadFork.outA <> waitLoad.enq
    loadFork.outB <> io.mainMem.memRdReq

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
      genIn = io.contextSaveReq.bits, genA = GenericMemoryRequest(p.mrp),
      genB = UInt(width = p.dataBits),
      forkA = {x: ValIndPair => makeWriteReq(x, UInt(p.writeIDBase))},
      forkB = {x: ValIndPair => x.value}
    )).io

    io.contextSaveReq <> saveFork.in
    saveFork.outA <> saveMemFork.in
    saveFork.outB <> waitSave.enq
    saveMemFork.outA <> io.mainMem.memWrReq
    saveMemFork.outB <> io.mainMem.memWrDat

    // sync on extmem write responses before issuing save response
    StreamJoin(
      inA = waitSave.deq, inB = io.mainMem.memWrRsp, genO = io.contextSaveRsp.bits,
      join = {(i: UInt, mr: GenericMemoryResponse) => i}
    ) <> io.contextSaveRsp

  } else throw new Exception("OoO ExtContextMem not yet supported")
}
