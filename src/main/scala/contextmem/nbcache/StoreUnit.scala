package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._
import TidbitsOCM._

class CacheLine(dataBits: Int, addrBits: Int) extends Bundle {
  val data = UInt(width = dataBits)
  val addr = UInt(width = addrBits)

  override def cloneType: this.type =
    new CacheLine(dataBits, addrBits).asInstanceOf[this.type]
}

class StoreUnit(
  val mrp: MemReqParams,  // MRP for main memory access
  val lineSizeBits: Int,  // bits in cacheline
  val chanIDBase: Int,    // base channel ID for memory requests
  val txns: Int           // # of outstanding memory requests
) extends Module {
  val io = new Bundle {
    // cachelines to write to main memory
    val writes = Decoupled(new CacheLine(lineSizeBits, mrp.addrWidth)).flip
    // checking for RAW hazards -- don't issue load while found in StoreUnit
    val checkAddr = UInt(INPUT, width = mrp.addrWidth)
    val checkFound = Bool(OUTPUT)
    // interface towards main mem
    val memWrReq = Decoupled(new GenericMemoryRequest(mrp))
    val memWrDat = Decoupled(UInt(width = mrp.dataWidth))
    val memWrRsp = Decoupled(new GenericMemoryResponse(mrp)).flip
  }
  if(lineSizeBits % mrp.dataWidth != 0)
    throw new Exception("Cacheline width not multiple of memsys data width")

  // convenience type and function definitions
  val cachelineData = UInt(width = lineSizeBits)
  val cachelineAddr = UInt(width = mrp.addrWidth)
  class IDAndCacheLine extends Bundle {
    val cacheline = new CacheLine(lineSizeBits, mrp.addrWidth)
    val id = UInt(width = log2Up(txns))

    override def cloneType: this.type =
      new IDAndCacheLine().asInstanceOf[this.type]
  }
  val idAndCacheLine = new IDAndCacheLine()
  def joinIDAndCacheLine(rq: CacheLine, id: UInt): IDAndCacheLine = {
    val rt = new IDAndCacheLine()
    rt.cacheline := rq
    rt.id := id
    return rt
  }

  // ID pool
  val idPool = Module(new ReqIDQueue(mrp.idWidth, txns, chanIDBase)).io
  idPool.doInit := Bool(false)  // IDEA make dynamically adjustable?

  // synchronize ID pool and incoming requests
  val readyReqs = StreamJoin(
    inA = io.writes, inB = idPool.idOut,
    genO = idAndCacheLine, join = joinIDAndCacheLine
  )

  // fork the synchronized requests into request and data streams
  val forkReqData = Module(new StreamFork(
    genIn = idAndCacheLine, genA = idAndCacheLine, genB = cachelineData,
    forkA = {x: IDAndCacheLine => x},
    forkB = {x: IDAndCacheLine => x.cacheline.data}
  )).io

  readyReqs <> forkReqData.in

  def toWrReq(req: IDAndCacheLine): GenericMemoryRequest = {
    val mr = new GenericMemoryRequest(mrp)
    mr.channelID := req.id
    mr.isWrite := Bool(true)
    mr.addr := req.cacheline.addr
    mr.numBytes := UInt(lineSizeBits/8)
    return mr
  }
  StreamFilter(forkReqData.outA, io.memWrReq.bits, toWrReq) <> io.memWrReq


  // wide queue for storing the received cacheline data
  val storeData = Module(new BRAMQueue(UInt(width = lineSizeBits), txns)).io
  forkReqData.outB <> storeData.enq
  StreamDownsizer(storeData.deq, mrp.dataWidth) <> io.memWrDat

  // type and structure to keep track of in-flight writes
  val pendingAddr = Vec.fill(txns) {Reg(init = UInt(width = mrp.addrWidth))}
  val pendingValid = Reg(init = Bits(0, txns))
  val setPending: UInt = UInt(width = txns)
  val clrPending: UInt = UInt(width = txns)
  setPending := UInt(0)
  clrPending := UInt(0)
  // offset the memsys IDs by chanIDBase when using as slot indices
  val newTxnSlot = idPool.idOut.bits - UInt(chanIDBase)
  val retTxnSlot = io.memWrRsp.bits.channelID - UInt(chanIDBase)

  // when transaction is accepted, write into pending
  when (idPool.idOut.valid & idPool.idOut.ready) {
    setPending := UIntToOH(newTxnSlot)
    pendingAddr(newTxnSlot) := io.writes.bits.addr
  }

  // clear transaction when completed
  io.memWrRsp.ready := Bool(true) // always ready to accept write responses
  idPool.idIn.valid := Bool(false)
  idPool.idIn.bits := io.memWrRsp.bits.channelID
  when(io.memWrRsp.ready & io.memWrRsp.valid) {
    // return ID to the pool
    // we assume the req ID pool always has room and don't check for ready
    idPool.idIn.valid := Bool(true)
    // clear valid bit for this transaction
    clrPending := UIntToOH(retTxnSlot)
  }

  // use bitmasks to set-clear the pending valid register
  pendingValid := (pendingValid | setPending) & ~clrPending

  // content-associative check on the write txns for RAW hazards
  val hitRAW = (0 until txns).map(
    {i => pendingValid(i) & pendingAddr(i) === io.checkAddr}
  )
  io.checkFound := Vec(hitRAW).toBits.orR
}
