package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsOCM._
import TidbitsStreams._

class BRAMContextMemParams(
  val depth: Int,
  val readLatency: Int,
  val writeLatency: Int,
  idBits: Int,
  dataBits: Int,
  mrp: MemReqParams
) extends ContextMemParams(idBits, dataBits, mrp)

class BRAMContextMem(p: BRAMContextMemParams) extends ContextMem(p) {
  val addrBits = log2Up(p.depth)
  // instantiate BRAM wrapper
  val bramw = Module(new BRAMWrapper(p)).io
  // connect load-save streams directly to BRAM wrapper ports by default
  io.contextLoadReq <> bramw.contextLoadReq
  bramw.contextLoadRsp <> io.contextLoadRsp
  io.contextSaveReq <> bramw.contextSaveReq
  bramw.contextSaveRsp <> io.contextSaveRsp

  // set up a sequence generator for use in init/flush
  val addrgen = Module(new SequenceGenerator(addrBits)).io
  addrgen.start := Bool(false)
  addrgen.init := UInt(0)
  addrgen.count := UInt(p.depth)
  addrgen.step := UInt(1)
  addrgen.seq.ready := Bool(false)

  // use reducer for completion detection in init
  val initOK = Module(new StreamReducer(p.dataBits, 0, {_+_})).io
  initOK.start := Bool(false)
  initOK.byteCount := UInt(p.depth * p.dataBits/8)
  initOK.streamIn.valid := bramw.contextSaveRsp.valid
  initOK.streamIn.bits := UInt(0)

  // StreamWriter for flushing
  val flush = Module(new StreamWriter(new StreamWriterParams(
    streamWidth = p.dataBits, mem = p.mrp, chanID = 0
  ))).io
  flush.start := Bool(false)
  flush.baseAddr := io.contextBase
  flush.byteCount := UInt(p.depth * p.dataBits/8)
  flush.in.valid := bramw.contextLoadRsp.valid
  flush.in.bits := bramw.contextLoadRsp.bits.vectorVal
  // give the main mem write channel to the StreamWriter
  flush.req <> io.mainMem.memWrReq
  flush.wdat <> io.mainMem.memWrDat
  io.mainMem.memWrRsp <> flush.rsp

  val sActive :: sInit :: sFlush :: sFinished :: Nil = Enum(UInt(), 4)
  val regState = Reg(init = UInt(sActive))

  when (regState != sActive) {
    // disable extern context save/load pipes when in init or flush
    io.contextLoadRsp.valid := Bool(false)
    io.contextSaveRsp.valid := Bool(false)
    io.contextLoadReq.ready := Bool(false)
    io.contextSaveReq.ready := Bool(false)
  }

  switch(regState) {
    is(sActive) {
      when (io.start) {
        when (io.mode === SeyrekModes.START_INIT) {regState := sInit}
        .elsewhen (io.mode === SeyrekModes.START_FLUSH) {regState := sFlush}
      }
    }

    is(sInit) {
      // connect addrgen to contextSave requests
      addrgen.start := Bool(true)
      addrgen.seq.ready := bramw.contextSaveReq.ready
      bramw.contextSaveReq.bits.ind := addrgen.seq.bits
      bramw.contextSaveReq.bits.value := UInt(0)  // init value
      bramw.contextSaveReq.valid := addrgen.seq.valid
      // connect contextSave responses to reducers to count them
      initOK.start := Bool(true)
      bramw.contextSaveRsp.ready := initOK.streamIn.ready

      when (initOK.finished) { regState := sFinished}
    }

    is(sFlush) {
      // TODO implement flush logic with main memory accesses
      // connect addrgen to contextLoad requests
      addrgen.start := Bool(true)
      addrgen.seq.ready := bramw.contextLoadReq.ready
      bramw.contextLoadReq.bits.ind := addrgen.seq.bits
      bramw.contextLoadReq.bits.value := UInt(0) // old context unused
      bramw.contextLoadReq.valid := addrgen.seq.valid
      // StreamWriter generate main mem write requests from load responses
      flush.start := Bool(true)
      bramw.contextLoadRsp.ready := flush.in.ready

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
  val readRspQ = Module(new Queue(io.contextLoadRsp.bits, p.readLatency+2)).io
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
  val writeRspQ = Module(new Queue(io.contextSaveRsp.bits, p.writeLatency+2)).io
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
