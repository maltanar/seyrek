package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsOCM._
import TidbitsStreams._

class RowMajorBackendIO(p: SeyrekParams) extends Bundle with SeyrekCtrlStat {
  val csr = new CSRSpMV(p).asInput
  // output to frontend -- length of each row, in order
  val rowLen = Decoupled(p.i)
  // output to frontend -- work units
  val workUnits = Decoupled(p.wu)
  // input from frontend -- received results, row-value pairs
  val results = Decoupled(p.vi).flip
  // context init
  val contextReqCnt = UInt(INPUT, 10)
  // memory ports
  val mainMem = Vec.fill(p.portsPerPE) {new GenericMemoryMasterPort(p.mrp)}
}


class RowMajorBackend(p: SeyrekParams) extends Module {
  val io = new RowMajorBackendIO(p)

  // instantiate result writer -- TODO parametrize type?
  val resWriter = Module(new ResultWriterSimple(p)).io
  io.csr <> resWriter.csr

  // give reducer results to result writer
  io.results <> resWriter.results

  // give the memory write port to the result writer
  resWriter.memWrReq <> io.mainMem(0).memWrReq
  resWriter.memWrDat <> io.mainMem(0).memWrDat
  io.mainMem(0).memWrRsp <> resWriter.memWrRsp

  // completion determination is by the result writer
  // TODO this may change for other x readers -- init phase may happen
  resWriter.start := io.start
  resWriter.mode := io.mode
  io.finished := resWriter.finished

  // set up the multichannel memory system
  val memsys = Module(new MultiChanMultiPort(p.mrp, p.portsPerPE,
    chans = p.chanConfig))

  for(i <- 0 until p.portsPerPE) {
    memsys.io.memReq(i) <> io.mainMem(i).memRdReq
    io.mainMem(i).memRdRsp <> memsys.io.memRsp(i)
  }

  // - if the platform does not return same ID reqs in-order, we need a read
  //   order cache. in this case throttling is not necessary, since the #
  //   of outstanding reqs naturally throttles the StreamReader.
  val needReadOrder: Boolean = !p.mrp.sameIDInOrder

  // instantiate StreamReaders for fetching the sequential SpMV streams
  // these will be feeding the frontend with data
  val readRowPtr = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.indWidth, fifoElems = 128, mem = p.mrp, maxBeats = 8,
    disableThrottle = needReadOrder, readOrderCache = needReadOrder,
    readOrderTxns = memsys.getChanParams("ptrs").maxReadTxns,
    chanID = memsys.getChanParams("ptrs").chanBaseID,
    streamName = "ptrs"
  )))
  memsys.connectChanReqRsp("ptrs", readRowPtr.io.req, readRowPtr.io.rsp)

  val readColInd = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.indWidth, fifoElems = 256, mem = p.mrp, maxBeats = 8,
    disableThrottle = needReadOrder, readOrderCache = needReadOrder,
    readOrderTxns = memsys.getChanParams("inds").maxReadTxns,
    chanID = memsys.getChanParams("inds").chanBaseID,
    streamName = "inds"
  )))
  memsys.connectChanReqRsp("inds", readColInd.io.req, readColInd.io.rsp)

  val readNZData = Module(new StreamReader(new StreamReaderParams(
    streamWidth = p.valWidth, fifoElems = 256, mem = p.mrp, maxBeats = 8,
    disableThrottle = needReadOrder, readOrderCache = needReadOrder,
    readOrderTxns = memsys.getChanParams("nzdata").maxReadTxns,
    chanID = memsys.getChanParams("nzdata").chanBaseID,
    streamName = "nzdata"
  )))
  memsys.connectChanReqRsp("nzdata", readNZData.io.req, readNZData.io.rsp)

  val startRegular = io.start & io.mode === SeyrekModes.START_REGULAR
  // use the row pointers to generate row lengths with StreamDelta
  val rowlens = StreamDelta(readRowPtr.io.out)
  val rowLenQ = Module(new FPGAQueue(p.i, 2)).io
  // need two copies of the row length stream: one for backend, one for frontend
  StreamCopy(rowlens, io.rowLen, rowLenQ.enq)
  // generate row inds for backend (will be repeated to form rowInds)
  val rowIndNoRep = NaturalNumbers(p.indWidth, startRegular, io.csr.rows)
  // repeat to generate the row inds
  val rowInds = StreamRepeatElem(rowIndNoRep, rowLenQ.deq)  // the j stream

  // synchronize streams to form a v-i-i structure (Aij, i, j)
  // this constitutes the x load requests
  val nzAndColInd = StreamJoin(readNZData.io.out, readColInd.io.out, p.vi,
    {(a: UInt, b: UInt) => ValIndPair(a, b)}) // (Aij, j)
  val loadReqs = StreamJoin(nzAndColInd, rowInds, p.vii,
    {(vi: ValIndPair, j: UInt) => ValIndInd(vi.value, j, vi.ind)}
  )

  // instantiate input vector loader -- TODO parametrize with function from p
  val inpVecLoader = Module(
    new ExtInpVecLoader(p, memsys.getChanParams("inpvec").chanBaseID)
  ).io
  memsys.connectChanReqRsp("inpvec", inpVecLoader.mainMem.memRdReq,
    inpVecLoader.mainMem.memRdRsp)
  // TODO inpVecLoader may need to signal finish in init mode
  inpVecLoader.start := io.start
  inpVecLoader.mode := io.mode
  inpVecLoader.contextBase := io.csr.inpVec

  // run through x read to get work unit and send to frontend
  loadReqs <> inpVecLoader.loadReq
  inpVecLoader.loadRsp <> io.workUnits

  val bytesVal = UInt(p.valWidth / 8)
  val bytesInd = UInt(p.indWidth / 8)

  // TODO these byte widths won't work if we are using non-byte-sized vals/inds

  readRowPtr.io.start := startRegular
  readRowPtr.io.baseAddr := io.csr.rowPtr
  readRowPtr.io.byteCount := bytesInd * (io.csr.rows + UInt(1))

  readColInd.io.start := startRegular
  readColInd.io.baseAddr := io.csr.colInd
  readColInd.io.byteCount := bytesInd * io.csr.nz

  readNZData.io.start := startRegular
  readNZData.io.baseAddr := io.csr.nzData
  readNZData.io.byteCount := bytesVal * io.csr.nz
}


// TODO separate into own source file?
class RowMajorResultWriterIO(p: SeyrekParams) extends Bundle with SeyrekCtrlStat {
  val csr = new CSRSpMV(p).asInput
  // received results, row-value pairs
  val results = Decoupled(p.vi).flip
  // req - rsp interface for memory writes
  val memWrReq = Decoupled(new GenericMemoryRequest(p.mrp))
  val memWrDat = Decoupled(UInt(width = p.mrp.dataWidth))
  val memWrRsp = Decoupled(new GenericMemoryResponse(p.mrp)).flip
}

// simple result writer; just send writes one and one to the memory system
// no write combining, no bursts, val width must match mem sys data width
class ResultWriterSimple(p: SeyrekParams) extends Module {
  if(p.mrp.dataWidth != p.valWidth)
  throw new Exception("ResultWriterSimple needs valWidth = mem dataWidth")

  val io = new RowMajorResultWriterIO(p)
  val startRegular = io.start & (io.mode === SeyrekModes.START_REGULAR)

  val writeMemFork = Module(new StreamFork(
    genIn = io.results.bits, genA = p.i, genB = p.v,
    forkA = {x: ValIndPair => x.ind},
    forkB = {x: ValIndPair => x.value}
  )).io

  io.results <> writeMemFork.in
  // TODO make write channel ID parametrizable?
  val wr = WriteArray(writeMemFork.outA, io.csr.outVec, UInt(0), p.mrp)
  wr <> io.memWrReq
  writeMemFork.outB <> io.memWrDat

  // count write completes to determine finished =============================

  io.memWrRsp.ready := Bool(true) // always ready to accept write resps

  val regCompletedRows = Reg(init = UInt(0, 32))
  val regStart = Reg(next = startRegular)
  when(!regStart & startRegular) {regCompletedRows := UInt(0)}
  .elsewhen (io.memWrRsp.ready & io.memWrRsp.valid) {
    regCompletedRows := regCompletedRows + UInt(1)
  }

  io.finished := Mux(startRegular, regCompletedRows === io.csr.rows, Bool(true))
}
