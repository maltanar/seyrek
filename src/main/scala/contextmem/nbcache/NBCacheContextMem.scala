package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._
import TidbitsOCM._

// note that this nonblocking cache requires a somewhat special scheduler
// - same cacheline must not enter while pending miss

class NBCacheContextMemParams(
  val numWordsInLine: Int,  // number of data words per cacheline
  val numLines: Int,        // number of lines in cache
  val readTxns: Int,        // number of outstanding main mem reads
  val writeTxns: Int,       // number of outstanding main mem writes
  idBits: Int,
  dataBits: Int,
  chanID: Int,
  mrp: MemReqParams
) extends ContextMemParams(idBits, dataBits, chanID, mrp)

class NBCacheContextMem(p: NBCacheContextMemParams) extends ContextMem(p) {
  val inOrder: Boolean = false
  // convenience types and functions for the cache
  val txns = p.readTxns + 1
  val idBits = log2Up(txns)
  val lineSizeBits: Int = dataBits * numWordsInLine
  val cacheBlockIDWidth = log2Up(p.numLines)
  val cacheBlockID = UInt(width = cacheBlockIDWidth)
  val cacheOffsetWidth = log2Up(numWordsInLine)
  val cacheOffset = UInt(width = cacheOffsetWidth)
  val cacheTagWidth = p.idWidth - (cacheBlockIDWidth + cacheOffsetWidth)
  val cacheTag = UInt(width = cacheTagWidth)
  def getTag(elem: UInt) = {elem(p.idWidth-1, cacheOffsetWidth+cacheBlockIDWidth)}
  def getBlockID(elem: UInt) = {elem(cacheOffsetWidth+cacheBlockIDWidth-1, cacheOffsetWidth)}
  def getOffset(elem: UInt) = {elem(cacheOffsetWidth-1, 0)}

  class CacheReq extends Bundle {
    val tag = UInt(width = cacheTagWidth)
    val line = UInt(width = cacheBlockIDWidth)
    val offset = UInt(width = cacheOffsetWidth)
    val reqid = UInt(width = idBits)
    override def cloneType: this.type = new CacheReq()
  }
  val cacheReq = new CacheReq()

  class CacheLookupRsp extends Bundle {
    val req = new CacheReq()
    val rspTag = UInt(width = cacheTagWidth)
    val rspValid = Bool()
    val rspLineData = UInt(width = lineSizeBits)
    override def cloneType: this.type = new CacheLookupRsp()
  }

  // instantiate and connect load and store units
  val lu = Module(new LoadUnit(
    mrp = mrp, lineSizeBits = lineSizeBits, chanIDBase = chanID,
    txns = readTxns
  )).io

  val su = Module(new StoreUnit(
    mrp = mrp, lineSizeBits = lineSizeBits, chanIDBase = chanID,
    txns = writeTxns
  )).io
  // avoid RAW hazards on the main memory level
  su.checkAddr := lu.checkAddr
  lu.checkFound := su.checkFound
  // main memory access for load and store units
  lu.memRdReq <> io.mainMem.memRdReq
  io.mainMem.memRdRsp <> lu.memRdRsp
  su.memWrReq <> io.mainMem.memWrReq
  su.memWrDat <> io.mainMem.memWrDat
  io.mainMem.memWrRsp <> su.memWrRsp

  // out-of-order "cloakroom" for storing the req contexts
  val idPool = Module(new ReqIDQueue(idBits, txns, 0)).io
  val cacheWait = Mem(io.contextLoadReq.bits, txns)

  // convert joined req-id pair into CacheReq for easier access to fields
  val readyReqs = StreamJoin(
    inA = io.contextLoadReq, inB = idPool.idOut,
    genO = cacheReq, join =
    {(rq: ValIndPair, id: UInt) => {
      val ret = new CacheReq()
      ret.tag := getTag(rq.ind)
      ret.line := getBlockID(rq.ind)
      ret.offset := getOffset(rq.ind)
      ret.reqid := id
      ret
    }}
  )

  // when a req enters the cache, preserve the read request data in
  // the cacheWait memory. this mem is indexed by the request ID, so the
  // memory line corresponding to an accepted txn's ID is always free
  when( readyReqs.ready & readyReqs.valid) {
    cacheWait(idPool.idOut.bits) := io.contextLoadReq.bits
  }

  // queue to store accepted requests, mostly here to improve timing
  val readyReqQ = FPGAQueue(readyReqs, 2)

  // instantiate data and tag memories,
  // +1 extra bit to make room for valid
  val tags = Module(new DualPortBRAM(cacheBlockIDWidth, cacheTagWidth + 1)).io
  val lines = Module(new DualPortBRAM(cacheBlockIDWidth, lineSizeBits)).io
  // give names to tag and data ports for easier access
  val tagRead = tags.ports(0)
  val tagWrite = tags.ports(1)  // only used for read replacement
  val lineRead = lines.ports(0)
  val lineWrite = lines.ports(1) // shared between writes & read replacement

  tagRead.req.writeEn := Bool(false)
  lineRead.req.writeEn := Bool(false)

  // TODO lookup & result queue -- handshake across latency


  // TODO activate appropriate units as result of lookup
  // TODO logic for tracking pending misses
  // TODO logic for handling load responses and pending misses
  // TODO remove context from cloakroom and issue response

  // TODO cache write port

}
