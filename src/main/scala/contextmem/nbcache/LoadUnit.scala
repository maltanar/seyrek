package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._
import TidbitsOCM._

// for now, we use a ReadOrderCache to make sure all requests return in-order,
// which will decrease efficiency somewhat
// TODO add out-of-order load handling for the load unit

class LoadUnit(
  val mrp: MemReqParams,  // MRP for main memory access
  val lineSizeBits: Int,  // bits in cacheline
  val chanIDBase: Int,    // base channel ID for memory requests
  val txns: Int           // # of outstanding memory requests
) extends Module {
  val io = new Bundle {
    // requests to load cacheline from main memory & responses
    val loadReq = Decoupled(UInt(width = mrp.addrWidth)).flip
    val loadRsp = Decoupled(new CacheLine(lineSizeBits, mrp.addrWidth))
    // checking for RAW hazards -- don't issue load while found in StoreUnit
    val checkAddr = UInt(OUTPUT, width = mrp.addrWidth)
    val checkFound = Bool(INPUT)
    // interface towards main memory
    val memRdReq = Decoupled(new GenericMemoryRequest(mrp))
    val memRdRsp = Decoupled(new GenericMemoryResponse(mrp)).flip
  }
  if(lineSizeBits % mrp.dataWidth != 0)
    throw new Exception("Cacheline width not multiple of memsys data width")
  val beatsForCacheline = lineSizeBits / mrp.dataWidth

  // instantiate the read order cache
  val roc = Module(new ReadOrderCache(new ReadOrderCacheParams(
    mrp = mrp, maxBurst = beatsForCacheline, outstandingReqs = txns,
    chanIDBase = chanIDBase, outputStreamID = 0
  ))).io

  // TODO expose ID queue reinit for the read order cache?
  roc.doInit := Bool(false)
  // mem req-rsp directly controlled by the ROC
  roc.reqMem <> io.memRdReq
  io.memRdRsp <> roc.rspMem
  // buffer incoming load requests in queue
  val loadReqQ = FPGAQueue(io.loadReq, 4)
  roc.reqOrdered.bits.addr := loadReqQ.bits
  roc.reqOrdered.bits.channelID := UInt(0) // ROC doesn't care anyway
  roc.reqOrdered.bits.isWrite := Bool(false)
  roc.reqOrdered.bits.numBytes := UInt(lineSizeBits/8)
  roc.reqOrdered.bits.metaData := UInt(0)
  // expose head of queue for RAW hazard checking
  io.checkAddr := loadReqQ.bits
  // don't issue load requests before we know they are hazard-free
  roc.reqOrdered.valid := loadReqQ.valid & !io.checkFound
  loadReqQ.ready := roc.reqOrdered.ready & !io.checkFound

  // "cloakroom" for keeping the pending addresses
  val pendingQ = Module(new FPGAQueue(loadReqQ.bits, txns)).io
  pendingQ.enq.bits := loadReqQ.bits
  pendingQ.enq.valid := loadReqQ.valid & loadReqQ.ready
  // filter out read data
  val rddat = ReadRespFilter(roc.rspOrdered)
  val cline = if(beatsForCacheline == 1) rddat else StreamUpsizer(rddat, lineSizeBits)

  StreamJoin(
    inA = pendingQ.deq, inB = cline, genO = io.loadRsp.bits,
    join = { (addr: UInt, data: UInt) =>
      val cacheline = new CacheLine(lineSizeBits, mrp.addrWidth)
      cacheline.data := data
      cacheline.addr := addr
      cacheline
    }
  ) <> io.loadRsp
}
