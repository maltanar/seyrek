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

  val numReadTxns = 8
  val numCacheTxns = 8
  val numLines = 4096

  if(!isPow2(numLines))
    throw new Exception("Cache lines must be power of two")

  val burstCount = 8
  val bytesPerMemWord = (p.mrp.dataWidth / 8)
  val bytesPerLine = bytesPerMemWord * burstCount
  val bytesPerElem = p.valWidth / 8
  val elemsPerLine = bytesPerLine / bytesPerElem
  val elemsPerMemWord = bytesPerMemWord / bytesPerElem
  val bitsPerLine = bytesPerLine * 8
  val bitsPerMemWord = p.mrp.dataWidth

  val numOffsBits = log2Up(elemsPerLine)
  val numIndBits = log2Up(numLines)
  val numTagBits = p.indWidth - (numOffsBits + numIndBits)

  // cache internal types
  class CacheReq extends CloakroomBundle(numCacheTxns) {
    // line number
    val lineNum = UInt(width = numIndBits)
    // line tag
    val tag = UInt(width = numTagBits)
    // word offset in line
    val offs = UInt(width = numOffsBits)

    override val printfStr = "id: %d lineNum: %d tag: %d offs: %d\n"
    override val printfElems = {() => Seq(id, lineNum, tag, offs)}

    override def cloneType: this.type = new CacheReq().asInstanceOf[this.type]
  }

  class CacheTagRsp extends CacheReq {
    // hit or miss?
    val isHit = Bool()
    // response data if hit
    val data = UInt(width = p.valWidth)

    override val printfStr = "id: %d lineNum: %d tag: %d hit: %d data: %d \n"
    override val printfElems = {() => Seq(id, lineNum, tag, isHit, data)}

    override def cloneType: this.type = new CacheTagRsp().asInstanceOf[this.type]
  }

  // cloneTypes for internal types
  val cacheReqID = UInt(width = log2Up(numCacheTxns))
  val cacheReq = new CacheReq()
  val cacheTagRsp = new CacheTagRsp()

  def printBundle(b: PrintableBundle) = {
    printf(b.printfStr, b.printfElems():_*)
  }

  // ==========================================================================
  // structures for keeping the cache state
  // TODO initialize tag and valid bits when in init mode
  // tags and valid bits
  val tagStore = Module(new DualPortBRAM(numIndBits, 1 + numTagBits)).io
  val tagLat = 1
  val tagRead = tagStore.ports(0)
  val tagWrite = tagStore.ports(1)

  tagRead.req.writeEn := Bool(false)
  tagRead.req.writeData := UInt(0)

  // tag init logic
  val regTagInitAddr = Reg(init = UInt(0, 1+numIndBits))

  io.finished := regTagInitAddr === UInt(numLines)

  when(io.mode === SeyrekModes.START_INIT & io.start & !io.finished) {
    regTagInitAddr := regTagInitAddr + UInt(1)
    tagRead.req.writeEn := Bool(true)
  } .elsewhen(!io.start) {
    regTagInitAddr := UInt(0)
  }

  // cacheline data
  val datAddrBits = numIndBits+numOffsBits
  val datStore = Module(new DualPortBRAM(datAddrBits, p.mrp.dataWidth)).io
  val datLat = 1
  val datRead = datStore.ports(0)
  val datWrite = datStore.ports(1)

  datRead.req.writeEn := Bool(false)

  // queues for storing intermediate results
  if(tagLat != datLat)
    throw new Exception("tag latency != data latency, needs fix")
  // TODO tagRespQ size should be max(tagLat, datLat) + 2
  val tagRespQ = Module(new FPGAQueue(cacheTagRsp, tagLat + 2)).io
  val missQ = Module(new FPGAQueue(cacheTagRsp, 2)).io
  val newReqQ = Module(new FPGAQueue(cacheReq, 2)).io
  val reqQ = Module(new FPGAQueue(cacheReq, 4)).io
  val respQ = Module(new FPGAQueue(cacheTagRsp, 4)).io

  // miss handler
  val missHandler = Module(new MissHandler(numReadTxns)).io
  missHandler.misses <> missQ.deq
  missHandler.contextBase := io.contextBase
  missHandler.tagPort <> tagWrite
  missHandler.dataPort <> datWrite

  missHandler.mainMem <> io.mainMem

  // mix in resolved requests into the request stream
  // IMPROVE use own cache port for resolving requests?
  val reqMix = Module(new Arbiter(cacheReq, 2)).io
  missHandler.resolved <> reqMix.in(0)
  newReqQ.deq <> reqMix.in(1)
  reqMix.out <> reqQ.enq

  // ==========================================================================
  // cloakroom -- don't carry around the entire request

  // turn the external (Aij, i, j, rl) into a cache request
  def viilToCacheReq(rq: ValIndIndLen): CacheReq = {
    val cr = new CacheReq()
    cr.tag := rq.j(p.indWidth-1, numOffsBits + numIndBits)
    cr.lineNum := rq.j(numOffsBits + numIndBits - 1, numOffsBits)
    cr.offs := rq.j(numOffsBits-1, 0)
    cr
  }

  def makeWU(origRq: ValIndIndLen, rsp: CacheTagRsp): WorkUnit = {
    val wu = new WorkUnitAndLen(p.valWidth, p.indWidth)
    wu.matrixVal := origRq.v
    wu.vectorVal := rsp.data
    wu.rowInd := origRq.i
    wu.rowLen := origRq.rl
    wu
  }

  val cloakroom = Module(new CloakroomLUTRAM(
    num = numCacheTxns, genA = io.loadReq.bits, genC = cacheTagRsp,
    undress = viilToCacheReq, dress = makeWU
  )).io

  io.loadReq <> cloakroom.extIn
  cloakroom.intOut <> newReqQ.enq

  respQ.deq <> cloakroom.intIn
  cloakroom.extOut <> io.loadRsp

  // for sanity checking loaded values under constrained emulation testing
  /*
  when(respQ.deq.ready & respQ.deq.valid) {
    val rs = respQ.deq.bits
    val rA = Cat(rs.tag, rs.lineNum, rs.offs)
    val expVal = (rA+UInt(1)) * UInt(10)
    when(expVal != rs.data) {
      printf("***ERROR! exp val %d found %d, resp:\n", expVal, rs.data)
      printf(rs.printfStr, rs.printfElems():_*)
    }
  }
  */

  // ==========================================================================
  // tag lookup logic

  // direct-mapped tag+data lookup with handshaking over latency
  // tag memory addrs have 1:1 correspondence with lines
  tagRead.req.addr := reqQ.deq.bits.lineNum
  // ...but a line spans several elements of the data memory
  val mAddrBase = reqQ.deq.bits.lineNum * UInt(burstCount)
  val mAddrOffs = reqQ.deq.bits.offs / UInt(elemsPerMemWord)
  datRead.req.addr := mAddrBase + mAddrOffs

  val tagHazard = (reqQ.deq.bits.lineNum === tagWrite.req.addr) & tagWrite.req.writeEn
  val canReadTag = (tagRespQ.count < UInt(2)) & !tagHazard

  val origReq = ShiftRegister(reqQ.deq.bits, tagLat)

  val tagMatch = tagRead.rsp.readData(numTagBits-1, 0) === origReq.tag
  val tagValid = tagRead.rsp.readData(numTagBits)
  val tagHit = tagMatch & tagValid

  reqQ.deq.ready := canReadTag

  // set up connections to the tagRespQ enq.
  tagRespQ.enq.valid := ShiftRegister(canReadTag & reqQ.deq.valid, tagLat)
  origReq <> tagRespQ.enq.bits
  tagRespQ.enq.bits.isHit := tagHit

  // data read is slightly trickier
  // if there are multiple inp.vec elements per word, need to choose subword
  if(elemsPerMemWord > 1) {
    val subWord = origReq.offs & UInt(elemsPerMemWord - 1)
    val wordStart = subWord * UInt(p.valWidth)
    val wordEnd = ((subWord + UInt(1)) * UInt(p.valWidth)) - UInt(1)
    tagRespQ.enq.bits.data := datRead.rsp.readData(wordEnd, wordStart)
  } else {
    tagRespQ.enq.bits.data := datRead.rsp.readData
  }

  // route tag lookup output into appropriate queue
  val tagRespDest = Seq(missQ.enq, respQ.enq)
  tagRespQ.deq <> DecoupledOutputDemux(tagRespQ.deq.bits.isHit, tagRespDest)

  // stats / performance counters
  val doMon = io.start
  io.cacheNewReq := StreamMonitor(newReqQ.deq, doMon, "cacheNewReq")

  // =========================================================================
  // debugging

  def monitorStream[T <: PrintableBundle](name: String, stream: DecoupledIO[T]) = {
    when(stream.valid & stream.ready) {
      printf("txn on " + name + ": ")
      printBundle(stream.bits)
    }
  }

  val verboseDebug = false
  if(verboseDebug) {
    val queues = Seq(newReqQ.enq, reqQ.enq, tagRespQ.enq, missQ.enq, respQ.enq, missHandler.misses, missHandler.resolved)
    val names = Seq("newReqQ", "req", "tagResp", "miss", "resp", "miss in", "miss out")
    for((q,n) <- queues zip names) {
      PrintableBundleStreamMonitor(q, Bool(true), "+"+n, true)
    }

/*
    when(io.start & io.mode === SeyrekModes.START_REGULAR) {
      printf("queue counts: \n")
      printf("reqQ %d, tagRespQ %d, missQ %d, respQ %d \n",
        reqQ.count, tagRespQ.count, missQ.count, respQ.count
      )
      printf("=================================================================\n")
    }
    */
  }

  //==========================================================================
  // miss handler -- IMPROVE spearate out as own external module?
  class MissHandler(txns: Int) extends Module {
    val io = new Bundle {
      // incoming misses
      val misses = Decoupled(cacheReq).flip
      // resolved requests
      val resolved = Decoupled(cacheReq)
      // pointer to start of x
      val contextBase = UInt(INPUT, width = p.mrp.addrWidth)
      // access to tag and data memories
      val tagPort = new OCMMasterIF(numTagBits+1, numTagBits+1, numIndBits)
      val dataPort = new OCMMasterIF(bitsPerMemWord, bitsPerMemWord, datAddrBits)
      // checking against in-progress hits to lines
      val lineToCheck = UInt(OUTPUT, numIndBits)
      val isLineInUse = Bool(INPUT)
      // access to main memory for issuing load requests
      val mainMem = new GenericMemoryMasterPort(p.mrp)
    }
    class PendingMiss extends CacheReq {
      val missID = UInt(width = log2Up(txns))

      override def cloneType: this.type = new PendingMiss().asInstanceOf[this.type]
    }
    val pendingMiss = new PendingMiss()
    // shorthands for main memory access
    val mreqQ = Module(new FPGAQueue(new GenericMemoryRequest(p.mrp), 2)).io
    val mrspQ = Module(new FPGAQueue(new GenericMemoryResponse(p.mrp), 2)).io
    mreqQ.deq <> io.mainMem.memRdReq
    io.mainMem.memRdRsp <> mrspQ.enq

    val mreq = mreqQ.enq
    val mrsp = mrspQ.deq

    // queue for storing conflicting cache misses
    val conflictQ = Module(new FPGAQueue(cacheReq, 16)).io
    val mixMiss = Module(new Arbiter(cacheReq, 2)).io
    io.misses <> mixMiss.in(0)
    conflictQ.deq <> mixMiss.in(1)
    val missHead = FPGAQueue(mixMiss.out, 2)

    // miss queues, where the misses wait for the load to complete
    val pendingMissQ = Module(new MultiChanQueueBRAM(
      gen = pendingMiss, chans = txns, elemsPerChan = 32,
      getChan = {p: PendingMiss => p.missID}
    )).io

    // registers for keeping the status of each miss load
    val loadValid = Reg(init = Bits(0, txns))
    val loadLine = Vec.fill(txns) {Reg(init = UInt(0, numIndBits))}
    val loadTag = Vec.fill(txns) {Reg(init = UInt(0, numTagBits))}
    val loadPrg = Vec.fill(txns) {Reg(init = UInt(0, 1+log2Up(burstCount)))}

    val hasFreeSlot = orR(~loadValid)
    val freePos = PriorityEncoder(~loadValid)
    val doAdd = Bool()
    val doClr = Bool()
    val clrPos = UInt(width = log2Up(txns))
    doAdd := Bool(false)
    doClr := Bool(false)

    // produce masks to allow simultaneous write+clear
    val writeMask = Mux(doAdd, UIntToOH(freePos), Bits(0, txns))
    val clearMask = Mux(doClr, ~(UIntToOH(clrPos)), ~Bits(0, txns))

    loadValid := (loadValid | writeMask) & clearMask

    // list of completed loads
    val completedQ = Module(new FPGAQueue(UInt(width = log2Up(txns)), 4)).io

    // ========================================================================

    // check incoming misses for line conflicts and tag match
    val lineMatch = Vec((0 until txns).map(i => loadLine(i) === missHead.bits.lineNum & loadValid(i))).toBits
    val tagMatch = Vec((0 until txns).map(i => loadTag(i) === missHead.bits.tag & loadValid(i))).toBits

    val tagAndLineMatch = lineMatch & tagMatch & loadValid
    val isConflict = (lineMatch & ~tagMatch & loadValid).orR
    val isExisting = tagAndLineMatch.orR
    val hitPos = PriorityEncoder(tagAndLineMatch)

    // conflicts cannot enter, even if there is a free slot --
    // they move into their own queue, called conflictQ, to wait
    val enterAsNew = !isExisting & !isConflict & hasFreeSlot & pendingMissQ.in.ready & mreq.ready
    val enterAsExisting = isExisting & pendingMissQ.in.ready
    val enterAsConflict = isConflict & conflictQ.enq.ready

    missHead.ready := enterAsConflict | enterAsNew | enterAsExisting

    conflictQ.enq.valid := missHead.valid & isConflict
    conflictQ.enq.bits := missHead.bits

    // move accepted miss into appropriate miss queue
    pendingMissQ.in.valid := missHead.valid & (enterAsNew | enterAsExisting)
    pendingMissQ.in.bits := missHead.bits
    pendingMissQ.in.bits.missID := Mux(isExisting, hitPos, freePos)

    // sanity check: miss must enter as only one kind
    when(missHead.valid & missHead.ready) {
      val entryType = Cat(enterAsNew, enterAsConflict, enterAsExisting)
      when(PopCount(entryType) != UInt(1)) {
        printf("***ERROR! Multiple entry: ")
        printf("enterAsNew = %d, ", enterAsNew)
        printf("enterAsConflict = %d, ", enterAsConflict)
        printf("enterAsExisting = %d, ", enterAsExisting)
        printf("\n for req: ")
        printf(missHead.bits.printfStr, missHead.bits.printfElems():_*)
        printf("lineMatch: %b \n", lineMatch)
        printf("tagMatch: %b \n", tagMatch)
        printf("CAM status: \n")
        for(i <- 0 until txns) {
          printf("entry %d: valid = %d line = %d tag = %d prg = %d\n", UInt(i),
            loadValid(i), loadLine(i), loadTag(i), loadPrg(i)
          )
        }
      }
    }

    // prepare for issuing mem req for the missed cacheline
    mreq.valid := doAdd
    mreq.bits.channelID := UInt(chanIDBase) + freePos
    mreq.bits.isWrite := Bool(false)
    val theLine = Cat(missHead.bits.tag, missHead.bits.lineNum)
    mreq.bits.addr := io.contextBase + theLine * UInt(bytesPerLine)
    mreq.bits.numBytes := UInt(bytesPerLine)
    mreq.bits.metaData := UInt(0)

    when(missHead.ready & missHead.valid & enterAsNew) {
      // add new miss entry into the table
      // write into CAM
      doAdd := Bool(true)
      loadLine(freePos) := missHead.bits.lineNum
      loadTag(freePos) := missHead.bits.tag
      loadPrg(freePos) := UInt(0)
    }

    // =======================================================================
    // handle read responses from main memory
    val mrspID = mrsp.bits.channelID - UInt(chanIDBase)
    val rspsReceived = loadPrg(mrspID)
    val targetLine = loadLine(mrspID)
    // signal ready to memRdRsp if line is not in use
    io.lineToCheck := targetLine
    mrsp.ready := !io.isLineInUse & completedQ.enq.ready
    // set up tag write
    io.tagPort.req.writeEn := Bool(false)
    io.tagPort.req.addr := targetLine
    io.tagPort.req.writeData := Cat(Bool(false), loadTag(mrspID))
    // set up data write
    io.dataPort.req.writeEn := Bool(false)
    val wAddrBase = targetLine * UInt(burstCount)
    val wAddrOffs = rspsReceived
    io.dataPort.req.addr := wAddrBase + wAddrOffs
    io.dataPort.req.writeData := mrsp.bits.readData
    // set up signals for completion
    completedQ.enq.bits := mrspID
    completedQ.enq.valid := Bool(false)


    when(mrsp.ready & mrsp.valid) {
      // activate data write
      io.dataPort.req.writeEn := Bool(true)
      // increment progress counter
      rspsReceived := rspsReceived + UInt(1)
      when(rspsReceived === UInt(0)) {
        // write valid = 0 when first response received
        io.tagPort.req.writeEn := Bool(true)
      } .elsewhen(rspsReceived === UInt(burstCount-1)) {
        // last burst beat received -- data is now valid
        io.tagPort.req.writeEn := Bool(true)
        io.tagPort.req.writeData := Cat(Bool(true), loadTag(mrspID))
        // signal completed, add mrspID to completedQ
        completedQ.enq.valid := Bool(true)
      }
    }

    // =======================================================================
    // flush pending requests when load finished
    val complHeadID = completedQ.deq.bits

    clrPos := complHeadID
    pendingMissQ.outSel := complHeadID

    io.resolved.valid := pendingMissQ.out.valid & completedQ.deq.valid
    io.resolved.bits := pendingMissQ.out.bits
    pendingMissQ.out.ready := io.resolved.ready & completedQ.deq.valid

    completedQ.deq.ready := Bool(false)

    // use a small counter to ensure that we have seen all the misses
    val regEnsureCompl = Reg(init = UInt(0, 3))
    val likeCompl = completedQ.deq.valid & !pendingMissQ.out.valid
    val ensureWaitCycles = 4

    when(likeCompl) {
      regEnsureCompl := regEnsureCompl + UInt(1)
      when(regEnsureCompl === UInt(ensureWaitCycles)) {
        // when we are sure all misses have been handled, clear this miss entry
        regEnsureCompl := UInt(0)
        completedQ.deq.ready := Bool(true)
        doClr := Bool(true)
      }
    }

    // ==========================================================================
    // debug
    val verboseDebug = false
    if(verboseDebug) {
      when(missHead.ready & missHead.valid & !isExisting) {
        when(!isConflict) {
          printf("New miss in handler: ")
          printf("mid = %d line = %d tag   = %d \n", freePos,
          missHead.bits.lineNum, missHead.bits.tag)
        } .otherwise {
          printf("Conflict miss, redir to conflictQ\n")
        }
      }

      when(mrsp.ready & mrsp.valid) {
        printf("writing miss data, mid %d addr %d data %d prg %d \n",
          mrspID, wAddrBase+wAddrOffs, mrsp.bits.readData, rspsReceived
        )
        when(rspsReceived === UInt(0)) {
          printf("invalidatig tag for line %d \n", targetLine)
        } .elsewhen(rspsReceived === UInt(burstCount-1)) {
          printf("signalling completion for line %d \n", targetLine)
        }
      }
    }
  }
}
