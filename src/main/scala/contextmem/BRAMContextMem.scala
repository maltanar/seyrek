package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsOCM._
import TidbitsStreams._

// TODO make init range and value customizable
// TODO make flush range customizable

class BRAMContextMemParams(
  val depth: Int,
  val readLatency: Int,
  val writeLatency: Int,
  idBits: Int,
  dataBits: Int,
  chanID: Int,
  mrp: MemReqParams
) extends ContextMemParams(idBits, dataBits, chanID, mrp)

class BRAMContextMem(p: BRAMContextMemParams) extends ContextMem(p) {
  val inOrder = true
  val addrBits = log2Up(p.depth)

  io.finished := Bool(false)
  io.mainMem.memRdReq.valid := Bool(false)
  io.mainMem.memRdRsp.ready := Bool(false)

  // instantiate BRAM wrapper
  val bramw = Module(new BRAMWrapper(p)).io

  // components for init and the save port
  val initReqQ = Module(new FPGAQueue(io.contextSaveReq.bits, 2)).io
  val initRspQ = Module(new FPGAQueue(io.contextSaveRsp.bits, 2)).io
  val saveSel = UInt(width = 1)
  saveSel := UInt(0)
  val saveReqs = Seq(io.contextSaveReq, initReqQ.deq)
  val saveRsps = Seq(io.contextSaveRsp, initRspQ.enq)
  DecoupledInputMux(saveSel, saveReqs) <> bramw.contextSaveReq
  bramw.contextSaveRsp <> DecoupledOutputDemux(saveSel, saveRsps)
  // sequence generator for init addresses
  val genInit = Module(new SequenceGenerator(addrBits)).io
  genInit.start := Bool(false)
  genInit.init := UInt(0)
  genInit.count := UInt(p.depth)
  genInit.step := UInt(1)
  genInit.seq.ready := initReqQ.enq.ready
  initReqQ.enq.valid := genInit.seq.valid
  initReqQ.enq.bits.value := UInt(0)
  initReqQ.enq.bits.ind := genInit.seq.bits
  // reducer for init completion detection
  val finInit = Module(new StreamReducer(p.dataBits, 0, {_+_})).io
  finInit.start := Bool(false)
  finInit.byteCount := UInt(p.depth * p.dataBits/8)
  finInit.streamIn.valid := initRspQ.deq.valid
  finInit.streamIn.bits := UInt(0)
  initRspQ.deq.ready := finInit.streamIn.ready

  // components for flush and the load port
  val flushReqQ = Module(new FPGAQueue(io.contextLoadReq.bits, 2)).io
  val flushRspQ = Module(new FPGAQueue(io.contextLoadRsp.bits, 2)).io
  val loadSel = UInt(width = 1)
  loadSel := UInt(0)
  val loadReqs = Seq(io.contextLoadReq, flushReqQ.deq)
  val loadRsps = Seq(io.contextLoadRsp, flushRspQ.enq)
  DecoupledInputMux(loadSel, loadReqs) <> bramw.contextLoadReq
  bramw.contextLoadRsp <> DecoupledOutputDemux(loadSel, loadRsps)
  // sequence generator for flush addresses
  val genFlush = Module(new SequenceGenerator(addrBits)).io
  genFlush.start := Bool(false)
  genFlush.init := UInt(0)
  genFlush.count := UInt(p.depth)
  genFlush.step := UInt(1)
  genFlush.seq.ready := flushReqQ.enq.ready
  flushReqQ.enq.valid := genFlush.seq.valid
  flushReqQ.enq.bits.value := UInt(0)
  flushReqQ.enq.bits.ind := genFlush.seq.bits
  // StreamWriter for flushing
  val flush = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.dataBits, mem = p.mrp, chanID = p.chanID
  ))).io
  flush.start := Bool(false)
  flush.baseAddr := io.contextBase
  flush.byteCount := UInt(p.depth * p.dataBits/8)
  flush.in.valid := flushRspQ.deq.valid
  flush.in.bits := flushRspQ.deq.bits.matrixVal
  flushRspQ.deq.ready := flush.in.ready
  // give the main mem write channel to the StreamWriter
  flush.req <> io.mainMem.memWrReq
  flush.wdat <> io.mainMem.memWrDat
  io.mainMem.memWrRsp <> flush.rsp

  val sActive :: sInit :: sFlush :: sFinished :: Nil = Enum(UInt(), 4)
  val regState = Reg(init = UInt(sActive))

  switch(regState) {
    is(sActive) {
      when (io.start) {
        when (io.mode === SeyrekModes.START_INIT) {regState := sInit}
        .elsewhen (io.mode === SeyrekModes.START_FLUSH) {regState := sFlush}
      }
    }

    is(sInit) {
      saveSel := UInt(1)  // set save channel muxes
      genInit.start := Bool(true)
      finInit.start := Bool(true)

      when (finInit.finished) { regState := sFinished}
    }

    is(sFlush) {
      loadSel := UInt(1)  // set load channel muxes
      genFlush.start := Bool(true)
      flush.start := Bool(true)

      when (flush.finished) { regState := sFinished}
    }

    is(sFinished) {
      io.finished := Bool(true)
      when (!io.start) {regState := sActive}
    }
  }
}

class BRAMWrapper(p: BRAMContextMemParams) extends Module {
  val io = new Bundle {
    // context load port
    val contextLoadReq = Decoupled(new ValIndPair(p.dataBits, p.idBits)).flip
    val contextLoadRsp = Decoupled(new WorkUnit(p.dataBits, p.idBits))
    // context save port
    val contextSaveReq = Decoupled(new ValIndPair(p.dataBits, p.idBits)).flip
    val contextSaveRsp = Decoupled(UInt(width = p.idBits))
  }
  val mem = Module(new DualPortBRAM(log2Up(p.depth), p.dataBits)).io

  val readPort = mem.ports(0)
  val writePort = mem.ports(1)

  // connect read port to load request-response queues
  val readRspQ = Module(new FPGAQueue(io.contextLoadRsp.bits, p.readLatency+2)).io
  val canDoRead = (readRspQ.count < UInt(p.readLatency))
  val doRead = canDoRead & io.contextLoadReq.valid

  io.contextLoadReq.ready := canDoRead
  readPort.req.addr := io.contextLoadReq.bits.ind
  readPort.req.writeEn := Bool(false)
  readPort.req.writeData := UInt(0)

  val readOK = ShiftRegister(doRead, p.readLatency)
  val readContext = ShiftRegister(io.contextLoadReq.bits, p.readLatency)

  readRspQ.enq.valid := readOK
  // these aren't really matrixVal and vectorVal, but old value and new contr.
  readRspQ.enq.bits.matrixVal := readPort.rsp.readData  // old value (O)
  readRspQ.enq.bits.vectorVal := readContext.value // new contribution (N)
  readRspQ.enq.bits.rowInd := readContext.ind
  readRspQ.deq <> io.contextLoadRsp

  // connect the write port to save request-response queues
  val writeRspQ = Module(new FPGAQueue(io.contextSaveRsp.bits, p.writeLatency+2)).io
  val canDoWrite = (writeRspQ.count < UInt(p.writeLatency))
  val doWrite = canDoWrite & io.contextSaveReq.valid

  io.contextSaveReq.ready := canDoWrite
  writePort.req.addr := io.contextSaveReq.bits.ind
  writePort.req.writeEn := doWrite
  writePort.req.writeData := io.contextSaveReq.bits.value

  val writeOK = ShiftRegister(doWrite, p.writeLatency)
  val writeInd = ShiftRegister(io.contextSaveReq.bits.ind, p.writeLatency)

  writeRspQ.enq.valid := writeOK
  writeRspQ.enq.bits := writeInd
  writeRspQ.deq <> io.contextSaveRsp
}
