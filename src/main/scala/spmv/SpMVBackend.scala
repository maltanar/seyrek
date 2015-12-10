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
  // context init
  val contextReqCnt = UInt(INPUT, 10)
  // context load port
  val contextLoadReq = Decoupled(p.vi).flip
  val contextLoadRsp = Decoupled(p.wu)
  // context save port
  val contextSaveReq = Decoupled(p.vi).flip
  val contextSaveRsp = Decoupled(p.i)
  // memory ports
  val mainMem = Vec.fill(p.portsPerPE) {new GenericMemoryMasterPort(p.mrp)}
}

class SpMVBackend(p: SeyrekParams) extends Module {
  val io = new SpMVBackendIO(p)

  val memsys = Module(new MultiChanMultiPort(p.mrp, p.portsPerPE,
    chans = p.chanConfig))

  for(i <- 0 until p.portsPerPE) {
    memsys.io.memReq(i) <> io.mainMem(i).memRdReq
    io.mainMem(i).memRdRsp <> memsys.io.memRsp(i)
  }

  // instantiate the context memory
  val contextmem = Module(
    // channel ID base is passed as argument to ctx.mem. constructor
    p.makeContextMemory(memsys.getChanParams("ctxmem-r"))
  )
  contextmem.io.contextReqCnt := io.contextReqCnt
  contextmem.io.start := io.start
  contextmem.io.mode := io.mode
  contextmem.io.contextBase := io.csc.outVec
  io.contextLoadReq <> contextmem.io.contextLoadReq
  contextmem.io.contextLoadRsp <> io.contextLoadRsp
  io.contextSaveReq <> contextmem.io.contextSaveReq
  contextmem.io.contextSaveRsp <> io.contextSaveRsp
  memsys.connectChanReqRsp("ctxmem-r", contextmem.io.mainMem.memRdReq,
    contextmem.io.mainMem.memRdRsp
  )
  // TODO write port sharing? this is the only write so far
  val ctxMemPort = memsys.getChanParams("ctxmem-w").port
  contextmem.io.mainMem.memWrReq <> io.mainMem(ctxMemPort).memWrReq
  contextmem.io.mainMem.memWrDat <> io.mainMem(ctxMemPort).memWrDat
  io.mainMem(ctxMemPort).memWrRsp <> contextmem.io.mainMem.memWrRsp

// - if the platform does not return same ID reqs in-order, we need a read
//   order cache. in this case throttling is not necessary, since the #
//   of outstanding reqs naturally throttles the StreamReader.
  val needReadOrder: Boolean = !p.mrp.sameIDInOrder

  // instantiate StreamReaders for fetching the sequential SpMV streams
  // these will be feeding the frontend with data
  val readColPtr = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.indWidth, fifoElems = 128, mem = p.mrp, maxBeats = 8,
    disableThrottle = needReadOrder, readOrderCache = needReadOrder,
    readOrderTxns = memsys.getChanParams("colptr").maxReadTxns,
    chanID = memsys.getChanParams("colptr").chanBaseID
  )))
  memsys.connectChanReqRsp("colptr", readColPtr.io.req, readColPtr.io.rsp)

  val readRowInd = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.indWidth, fifoElems = 256, mem = p.mrp, maxBeats = 8,
    disableThrottle = needReadOrder, readOrderCache = needReadOrder,
    readOrderTxns = memsys.getChanParams("rowind").maxReadTxns,
    chanID = memsys.getChanParams("rowind").chanBaseID
  )))
  memsys.connectChanReqRsp("rowind", readRowInd.io.req, readRowInd.io.rsp)

  val readNZData = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.valWidth, fifoElems = 256, mem = p.mrp, maxBeats = 8,
    disableThrottle = needReadOrder, readOrderCache = needReadOrder,
    readOrderTxns = memsys.getChanParams("nzdata").maxReadTxns,
    chanID = memsys.getChanParams("nzdata").chanBaseID
  )))
  memsys.connectChanReqRsp("nzdata", readNZData.io.req, readNZData.io.rsp)

  val readInpVec = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.valWidth, fifoElems = 128, mem = p.mrp, maxBeats = 8,
    disableThrottle = needReadOrder, readOrderCache = needReadOrder,
    readOrderTxns = memsys.getChanParams("inpvec").maxReadTxns,
    chanID = memsys.getChanParams("inpvec").chanBaseID
  )))
  memsys.connectChanReqRsp("inpvec", readInpVec.io.req, readInpVec.io.rsp)

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
