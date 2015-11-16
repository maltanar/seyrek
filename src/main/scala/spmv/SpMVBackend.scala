package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._

// TODOs for SpMVBackend:
// - instantiate and connect ContextMemory (BRAM, cache, DRAM)
// - add init and flush modes (for ContextMemory)
// - full system testing

class SpMVBackendIO(p: SeyrekParams) extends Bundle with SeyrekCtrlStat {
  val csc = new CSCSpMV(p).asInput
  // output to frontend
  val workUnits = Decoupled(p.wu)
  // context load port
  val contextLoadReq = Decoupled(p.vi).flip
  val contextLoadRsp = Decoupled(p.wu)
  // context save port
  val contextSaveReq = Decoupled(p.vi).flip
  val contextSaveRsp = Decoupled(p.i)
  // memory ports
  // TODO parametrize SpMV backend port count and mapping
  val reqSeq = new GenericMemoryMasterPort(p.mrp)
}

class SpMVBackend(p: SeyrekParams) extends Module {
  val io = new SpMVBackendIO(p)

  // instantiate the context memory
  val contextmem = Module(p.makeContextMemory())
  contextmem.io.start := io.start
  contextmem.io.mode := io.mode
  contextmem.io.contextBase := io.csc.outVec
  io.contextLoadReq <> contextmem.io.contextLoadReq
  contextmem.io.contextLoadRsp <> io.contextLoadRsp
  io.contextSaveReq <> contextmem.io.contextSaveReq
  contextmem.io.contextSaveRsp <> io.contextSaveRsp

  // instantiate StreamReaders for fetching the sequential SpMV streams
  // these will be feeding the frontend with data
  val readColPtr = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.indWidth, fifoElems = 128, mem = p.mrp, maxBeats = 8,
    chanID = 0
  )))

  val readRowInd = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.indWidth, fifoElems = 256, mem = p.mrp, maxBeats = 8,
    chanID = 1
  )))

  val readNZData = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.valWidth, fifoElems = 256, mem = p.mrp, maxBeats = 8,
    chanID = 2
  )))

  val readInpVec = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.valWidth, fifoElems = 128, mem = p.mrp, maxBeats = 8,
    chanID = 3
  )))

  // use the column pointers to generate column lengths with StreamDelta
  val colLens = StreamDelta(readColPtr.io.out)
  // repeat each input vector element <colLen> times
  val repeatedVec = StreamRepeatElem(readInpVec.io.out, colLens)
  // join up to create the outputs that the frontend expects
  val nzAndInd = StreamJoin(readNZData.io.out, readRowInd.io.out, p.vi,
    {(a: UInt, b: UInt) => ValIndPair(a, b)})
  def makeWorkUnit(vi: ValIndPair, v: UInt): WorkUnit = {
    WorkUnit(vi.value, v, vi.ind) }
  StreamJoin(nzAndInd, repeatedVec, p.wu, makeWorkUnit) <> io.workUnits

  // interleave all sequential streams onto a single memory port
  // TODO make the stream<->port matching more configurable
  // TODO make convenience function/object for generating several readers
  val readSeqIntl = Module(new ReqInterleaver(5, p.mrp)).io
  readColPtr.io.req <> readSeqIntl.reqIn(readColPtr.p.chanID)
  readRowInd.io.req <> readSeqIntl.reqIn(readRowInd.p.chanID)
  readNZData.io.req <> readSeqIntl.reqIn(readNZData.p.chanID)
  readInpVec.io.req <> readSeqIntl.reqIn(readInpVec.p.chanID)
  // TODO get ID range from contextmem + customize routing function
  contextmem.io.mainMem.memRdReq <> readSeqIntl.reqIn(contextmem.p.chanID)

  readSeqIntl.reqOut <> io.reqSeq.memRdReq

  // deinterleaver to seperate incoming read responses
  val readSeqDeintl = Module(new QueuedDeinterleaver(5, p.mrp, 4)).io
  io.reqSeq.memRdRsp <> readSeqDeintl.rspIn

  readSeqDeintl.rspOut(readColPtr.p.chanID) <> readColPtr.io.rsp
  readSeqDeintl.rspOut(readRowInd.p.chanID) <> readRowInd.io.rsp
  readSeqDeintl.rspOut(readNZData.p.chanID) <> readNZData.io.rsp
  readSeqDeintl.rspOut(readInpVec.p.chanID) <> readInpVec.io.rsp
  // TODO get ID range from contextmem + customize routing function
  readSeqDeintl.rspOut(contextmem.p.chanID) <> contextmem.io.mainMem.memRdRsp

  // write channel used only by the ContextMem
  // TODO parametrize write channel usage?
  contextmem.io.mainMem.memWrReq <> io.reqSeq.memWrReq
  contextmem.io.mainMem.memWrDat <> io.reqSeq.memWrDat
  io.reqSeq.memWrRsp <> contextmem.io.mainMem.memWrRsp

  // control signals for StreamReaders
  val startRegular = (io.mode === SeyrekModes.START_REGULAR) & io.start
  val bytesVal = UInt(p.valWidth / 8)
  val bytesInd = UInt(p.indWidth / 8)
  // TODO these byte widths won't work if we are using non-byte-sized vals/inds

  readColPtr.io.start := startRegular
  readColPtr.io.baseAddr := io.csc.colPtr
  readColPtr.io.byteCount := bytesInd * (io.csc.cols + UInt(1))

  readRowInd.io.start := startRegular
  readRowInd.io.baseAddr := io.csc.rowInd
  readRowInd.io.byteCount := bytesInd * io.csc.nz

  readNZData.io.start := startRegular
  readNZData.io.baseAddr := io.csc.nzData
  readNZData.io.byteCount := bytesVal * io.csc.nz

  readInpVec.io.start := startRegular
  readInpVec.io.baseAddr := io.csc.inpVec
  readInpVec.io.byteCount := bytesVal * io.csc.cols

  val seqReaders = Array[StreamReader](readColPtr, readRowInd, readNZData, readInpVec)

  io.finished := Bool(false)

  when (io.mode === SeyrekModes.START_REGULAR) {
    io.finished := seqReaders.map(x => x.io.finished).reduce(_ & _)
  } .otherwise {
    io.finished := contextmem.io.finished
  }
}
