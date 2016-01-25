package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._
import TidbitsOCM._

class NBDMInpVecCache(p: SeyrekParams, chanIDBase: Int) extends InpVecLoader(p) {
  val inOrder = false

  // TODO parametrize

  if(p.valWidth > p.mrp.dataWidth || p.mrp.dataWidth % p.valWidth != 0)
    throw new Exception("Unsupported valWidth:dataWidth ratio")

  val numReadTxns = 4
  val numCacheTxns = numReadTxns + 2
  val numLines = 1024

  if(!isPow2(numLines))
    throw new Exception("Cache lines must be power of two")

  val burstCount = 8
  val bytesPerMemWord = (p.mrp.dataWidth / 8)
  val bytesPerLine = bytesPerMemWord * burstCount
  val bytesPerElem = p.valWidth / 8
  val elemsPerLine = bytesPerLine / bytesPerElem
  val elemsPerMemWord = bytesPerMemWord / bytesPerElem
  val bitsPerLine = bytesPerLine * 8

  val numOffsBits = log2Up(elemsPerLine)
  val numIndBits = log2Up(numLines)
  val numTagBits = p.indWidth - (numOffsBits + numIndBits)

  // cache internal types
  class CacheReq extends Bundle {
    // cache internal ID
    val reqID = UInt(width = log2Up(numCacheTxns))
    // line number
    val lineNum = UInt(width = numIndBits)
    // line tag
    val tag = UInt(width = numTagBits)
    // word offset in line
    val offs = UInt(width = numOffsBits)

    override def cloneType: this.type = new CacheReq().asInstanceOf[this.type]
  }
  class CacheTagRsp extends Bundle {
    // hit or miss?
    val isHit = Bool()
    // data from original req:
    val req = new CacheReq()

    override def cloneType: this.type = new CacheTagRsp().asInstanceOf[this.type]
  }
  class CacheRsp extends Bundle {
    // cache internal ID
    val reqID = UInt(width = log2Up(numCacheTxns))
    // response data
    val data = UInt(width = p.valWidth)

    override def cloneType: this.type = new CacheRsp().asInstanceOf[this.type]
  }

  // cloneTypes for internal types
  val cacheRsp = new CacheRsp()
  val cacheReqID = UInt(width = log2Up(numCacheTxns))
  val cacheReq = new CacheReq()
  val cacheTagRsp = new CacheTagRsp()

  // miss handler -- TODO spearate out as own external module?
  class MissHandler(txns: Int) extends Module {
    val io = new Bundle {
      // incoming misses
      val misses = Decoupled(cacheReq).flip
      // resolved requests
      val resolved = Decoupled(cacheReq)
      // pointer to start of x
      val contextBase = UInt(INPUT, width = p.mrp.addrWidth)
      // access to tag and data memories
      val tagWritePort = new OCMMasterIF(numTagBits+1, numTagBits+1, numIndBits)
      val dataWritePort = new OCMMasterIF(bitsPerLine, bitsPerLine, numIndBits)
      // checking against in-progress hits to lines
      val lineToCheck = UInt(OUTPUT, numIndBits)
      val isLineInUse = Bool(INPUT)
      // access to main memory for issuing load requests
      val mainMem = new GenericMemoryMasterPort(p.mrp)
    }
    // miss queues, where the misses wait for the load to complete
    val missQ = Vec.fill(txns) {
      Module(new FPGAQueue(cacheReq, 4)).io
    }
    val missQEnqSel = UInt(width = log2Up(txns))
    val missQEnq = DecoupledInputMux(missQEnqSel, missQ.map(x => x.enq))
    val missQDeqSel = UInt(width = log2Up(txns))
    val missQDeq = DecoupledOutputDemux(missQDeqSel, missQ.map(x => x.deq))

    // registers for keeping the status of each miss load
    val loadValid = Reg(init = Bits(0, txns))
    val loadID = Vec.fill(txns) {Reg(init = UInt(0, log2Up(txns)))}
    val loadLine = Vec.fill(txns) {Reg(init = UInt(0, numIndBits))}
    val loadTag = Vec.fill(txns) {Reg(init = UInt(0, numTagBits))}
    val loadPrg = Vec.fill(txns) {Reg(init = UInt(0, 1+log2Up(burstCount)))}

    val hasFreeSlot = orR(~loadValid)
    val freeSlotInd = PriorityEncoder(~loadValid)

    // check incoming misses for line conflicts and tag match
    val missHead = io.misses.bits
    val lineMatch = (0 until txns).map(i => loadLine(i) === missHead.lineNum & loadValid(i))
    val tagMatch = (0 until txns).map(i => loadTag(i) === missHead.tag & loadValid(i))
    val tagMatchPos = PriorityEncoder(Vec(tagMatch).toBits)
    val isConflict = Vec(lineMatch).toBits.orR
    val isExisting = Vec(tagMatch).toBits.orR

    // conflicts cannot enter, even if there is a free slot
    io.misses.ready := isExisting | (!isConflict & hasFreeSlot)

    when(io.misses.ready & io.misses.valid & !isExisting) {
      // add new miss entry into the table
      // TODO issue load request to main memory
    }

    // TODO move accepted miss into appropriate miss queue

    // TODO produce masks for write & clear, and update valid bits

    // TODO handle read responses from main memory

    // TODO write valid = 0 when first response received
    // TODO remove load entry when all responses received
    // TODO drain miss queue when all responses are received

  }

  // ==========================================================================

  // cloakroom -- don't carry around the entire request
  val cloakroom = Mem(p.vii, numCacheTxns)
  val cacheReqIDPool = Module(
    new ReqIDQueue(log2Up(numCacheTxns), numCacheTxns, 0)).io

  // turn the external (Aij, i, j) into a cache request
  def makeCacheReq(rq: ValIndInd, rid: UInt): CacheReq = {
    val cr = new CacheReq()
    cr.reqID := rid
    cr.tag := rq.j(p.indWidth-1, numOffsBits + numIndBits)
    cr.lineNum := rq.j(numOffsBits + numIndBits - 1, numOffsBits)
    cr.offs := rq.j(numOffsBits-1, 0)
    cr
  }

  val readyReqs = StreamJoin(inA = io.loadReq, inB = cacheReqIDPool.idOut,
    genO = cacheReq, join = makeCacheReq
  )

  // add to cloakroom when request arrives
  when( readyReqs.ready & readyReqs.valid) {
    cloakroom(readyReqs.bits.reqID) := io.loadReq.bits
  }

  // requets ready to be processed
  val reqQ = Module(new FPGAQueue(cacheReq, 4)).io
  readyReqs <> reqQ.enq

  // responses ready to be processed
  val respQCapacity = 4
  val respQ = Module(new FPGAQueue(cacheRsp, respQCapacity)).io

  val readyResps = Module(new StreamFork(
    genIn = cacheRsp, genA = cacheReqID, genB = p.wu,
    forkA = {a: CacheRsp => a.reqID},
    forkB = {a: CacheRsp =>
      val newWU = new WorkUnit(p.valWidth, p.indWidth)
      newWU.vectorVal := a.data
      // other WU elements will be fetched from the cloakroom
      newWU
    }
  )).io

  respQ.deq <> readyResps.in
  readyResps.outA <> cacheReqIDPool.idIn
  readyResps.outB <> io.loadRsp

  // retrieve rowInd and matrixVal upon exit
  io.loadRsp.bits.rowInd := cloakroom(readyResps.in.bits.reqID).i
  io.loadRsp.bits.matrixVal := cloakroom(readyResps.in.bits.reqID).v

  // ==========================================================================
  // structures for keeping the cache state
  // TODO initialize tag and valid bits when in init mode
  // tags and valid bits
  val tagStore = Module(new DualPortBRAM(numIndBits, 1 + numTagBits)).io
  val tagLat = 1
  val tagRead = tagStore.ports(0)
  val tagWrite = tagStore.ports(1)

  tagRead.req.writeEn := Bool(false)
  tagWrite.req.writeEn := Bool(false)

  // cacheline data
  val datStore = Module(new DualPortBRAM(numIndBits, bitsPerLine)).io
  val datLat = 1
  val datRead = datStore.ports(0)
  val datWrite = datStore.ports(1)

  datRead.req.writeEn := Bool(false)
  datWrite.req.writeEn := Bool(false)

  // structure for tracking in-flight load requests
  // TODO

  // pending misses will be collected in these queues
  val pendingMiss = Vec.fill(numReadTxns) {
    Module(new FPGAQueue(cacheRsp, 4)).io
  }

  // for tracking in-flight hits; these lines should not be written to
  val pendingLineHits = Module(new SearchableQueue(UInt(width = numIndBits), 4)).io

  // queues for storing intermediate results
  val tagRespQ = Module(new FPGAQueue(cacheTagRsp, tagLat + 2)).io
  val hitQ = Module(new FPGAQueue(cacheTagRsp, 2)).io
  val missQ = Module(new FPGAQueue(cacheTagRsp, 2)).io

  // ==========================================================================
  // tag lookup logic

  // direct-mapped tag lookup with handshaking over latency
  tagRead.req.addr := reqQ.deq.bits.lineNum
  val origReq = ShiftRegister(reqQ.deq.bits, tagLat)
  val tagMatch = tagRead.rsp.readData(numTagBits-1, 0) === origReq.tag
  val tagValid = tagRead.rsp.readData(numTagBits)
  val tagHit = tagMatch & tagValid

  val canReadTag = tagRespQ.count < UInt(2)
  tagRespQ.enq.valid := ShiftRegister(canReadTag & reqQ.deq.valid, tagLat)
  tagRespQ.enq.bits.isHit := tagHit
  tagRespQ.enq.bits.req := origReq

  // add to pending hits when there is a hit
  val isNewHit = tagRespQ.enq.valid & tagRespQ.enq.ready & tagHit
  pendingLineHits.enq.valid := isNewHit
  pendingLineHits.enq.bits := origReq.lineNum

  // route tag lookup output into appropriate queues
  val tagRespDest = Seq(missQ.enq, hitQ.enq)
  tagRespQ.deq <> DecoupledOutputDemux(tagRespQ.deq.bits.isHit, tagRespDest)

  // supply cache hits
  // ==========================================================================

  // handshake over latency to fetch data and put into respQ
  // hitQ -> [mem] -> respQ
  val mAddrBase = hitQ.deq.bits.req.lineNum * UInt(burstCount)
  val mAddrOffs = hitQ.deq.bits.req.offs / UInt(elemsPerMemWord)
  datRead.req.addr := mAddrBase + mAddrOffs

  val canResp = respQ.count < UInt(respQCapacity - datLat)
  val origTagRspReq = ShiftRegister(hitQ.deq.bits.req, datLat)

  hitQ.deq.ready := canResp
  respQ.enq.valid := ShiftRegister(canResp & hitQ.deq.valid, datLat)
  respQ.enq.bits.reqID := origTagRspReq.reqID

  // if there are multiple inp.vec elements per word, need to choose subword
  if(elemsPerMemWord > 1) {
    val subWord = origTagRspReq.offs & UInt(elemsPerMemWord - 1)
    val wordStart = subWord * UInt(p.valWidth)
    val wordEnd = ((subWord + UInt(1)) * UInt(p.valWidth)) - UInt(1)
    respQ.enq.bits.data := datRead.rsp.readData(wordEnd, wordStart)
  } else {
    respQ.enq.bits.data := datRead.rsp.readData
  }

  // decrement hit-in-progress counter for this line
  pendingLineHits.deq.ready := respQ.enq.valid & respQ.enq.ready

  // sanity check: dequeued pending hit line and actually read line must match
  when(respQ.enq.valid & respQ.enq.ready) {
    when(pendingLineHits.deq.bits != origTagRspReq.lineNum) {
      printf("***ERROR! mismatch between expected and queued hit line #\n ")
    }
  }

  // ==========================================================================
  // issue requests for misses

  // TODO check if request already exists for tag OR as this line as dest.

  // ==========================================================================
  // handle miss responses from main memory

  // TODO check against line busy, update counters and tags as needed

}
