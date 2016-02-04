package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsOCM._
import TidbitsStreams._

class RowMajorFrontendIO(p: SeyrekParams) extends Bundle with SeyrekCtrlStat {
  val csr = new CSRSpMV(p).asInput
  // work units to the reducer
  val workUnits = Decoupled(p.wul).flip
  // results, row-value pairs
  val results = Decoupled(p.vi)
}

class DummyRowMajorFrontend(p: SeyrekParams) extends Module {
  val io = new RowMajorFrontendIO(p)

  val startRegular = io.start & io.mode === SeyrekModes.START_REGULAR

  io.workUnits.ready := Bool(true)
  val regWUCount = Reg(init = UInt(0, 32))

  when(startRegular & io.workUnits.ready & io.workUnits.valid) {
    regWUCount := regWUCount + UInt(1)
    //printf("wu count %d \n", regWUCount)
  }

  val wuFinished = regWUCount === io.csr.nz


  val seq = NaturalNumbers(p.indWidth, startRegular & wuFinished, io.csr.rows)

  io.results.valid := seq.valid
  io.results.bits.value := seq.bits
  io.results.bits.ind := seq.bits
  seq.ready := io.results.ready

  when(io.results.ready & io.results.valid & io.results.bits.ind === UInt(0)) {
    printf("Warning: using dummy frontend\n")
  }

}

class RowMajorFrontend(p: SeyrekParams) extends Module {
  val io = new RowMajorFrontendIO(p)

  // instantiate the semiring multiply op and the reducer
  val mul = Module(new ContextfulSemiringOp(p, p.makeSemiringMul)).io
  val red = Module(new RowMajorReducer(p)).io

  // (v, v, i) -> [queue] -> [mul] -> (n = v*v, i)
  FPGAQueue(io.workUnits, 2) <> mul.in

  // feed mul results as operands to reducer
  FPGAQueue(mul.out, 2) <> red.operands

  // add row numbers to create the (rownumber, rowlength) input to reducer
  val startRegular = io.start & io.mode === SeyrekModes.START_REGULAR

  // push out results from reducer
  red.results <> io.results
}

// note that the name "RowMajorFrontend" is a slight misnomer; the design will
// support small deviations from row-major (needed to support out-of-order
//  data returns from the x read)
class RowMajorReducer(p: SeyrekParams) extends Module {
  val verbose_debug = false
  val io = new Bundle {
    // partial products in
    val operands = Decoupled(p.vii).flip
    // results out
    val results = Decoupled(p.vi)
  }
  // op queue = b queue of the upsizer
  // upsizer for issuing add ops. inA from the adder, inB from the opQ
  val ups = Vec.fill(p.issueWindow) {Module(new RowMajorReducerUpsizer(p)).io}
  // demux for the upsizer stream inputs
  val upsEntrySel = UInt(width = log2Up(p.issueWindow))
  val upsEntry = DecoupledOutputDemux(upsEntrySel, ups.map(_.inA))
  upsEntrySel := upsEntry.bits.ind
  // round robin arbiter for popping the op pairs for the adder
  val opArb = Module(new RRArbiter(p.wu, p.issueWindow)).io
  // signal for controlling the enq index to the opQ's
  val opQEnqSel = UInt(width = log2Up(p.issueWindow))
  // demuxer for enq into op queues
  val opQEnq = DecoupledOutputDemux(opQEnqSel, ups.map(x => x.inB))

  for(i <- 0 until p.issueWindow) {
    ups(i).out <> opArb.in(i)
  }
  // count of each op queue, useful for debug
  val counts = Vec(ups.map(x => x.bCount))

  // the adder as defined by the semiring
  val add = Module(new ContextfulSemiringOp(p, p.makeSemiringAdd)).io
  // queue that keeps the operands with translated IDs
  val workQ = Module(new FPGAQueue(p.vi, 2)).io
  val addQ = Module(new FPGAQueue(p.wu, 2)).io  // adder inputs
  val resQ = Module(new FPGAQueue(p.vi, 2)).io  // adder results
  val resOpQ = Module(new FPGAQueue(p.vi, 2)).io  // adder results back to ops
  val retQ = Module(new FPGAQueue(p.vi, 2)).io  // rows ready to retire

  // rr arbiter for mixing 1- results into retQ
  val retQArb = Module(new RRArbiter(p.vi, 2)).io
  val oneQ = Module(new FPGAQueue(p.vi, 2)).io  // injected ones

  // scheduler bookkeeping
  // the row major reducer uses a scheduler to keep track of the status of each
  // row: may be several rows in the reducer at a time due to out-of-order
  val sched = Module(new CAM(entries = p.issueWindow, tag_bits = p.indWidth)).io
  val regNZLeft = Vec.fill(p.issueWindow) { Reg(init = UInt(0, p.indWidth)) }
  val regActualRowID = Vec.fill(p.issueWindow) { Reg(init = UInt(0, p.indWidth)) }

  // =========================================================================
  // entry into scheduler
  val entryHead = io.operands.bits
  val hasFree = sched.hasFree
  val freeSlot = sched.freeInd
  val isRowSingleElem = (entryHead.j === UInt(1))
  val hitSlot = PriorityEncoder(sched.hits)

  // check if this row is already being reduced
  sched.tag := entryHead.i

  // be prepared for adding to the scheduler
  sched.write := Bool(false)
  sched.write_tag := entryHead.i

  // determine whether this operand can enter the reducer, and in which mode
  // - new one-length row (write straight to the results)
  // - new regular row (add to sched, write to workQ)
  // - existing row (write to workQ)
  val enterAsNew1 = isRowSingleElem & oneQ.enq.ready
  val enterAsNewReg = !sched.hit & !isRowSingleElem & hasFree & workQ.enq.ready
  val enterAsExisting = sched.hit & workQ.enq.ready

  io.operands.ready := enterAsNew1 | enterAsNewReg | enterAsExisting

  // set up oneQ entry
  oneQ.enq.valid := io.operands.valid & enterAsNew1
  oneQ.enq.bits.ind := entryHead.i
  oneQ.enq.bits.value := entryHead.v

  // set up workQ entry
  workQ.enq.valid := io.operands.valid & (enterAsNewReg | enterAsExisting)
  workQ.enq.bits.ind := Mux(sched.hit, hitSlot, freeSlot)
  workQ.enq.bits.value := entryHead.v

  // add to scheduler when appropriate
  when(io.operands.ready & io.operands.valid & enterAsNewReg) {
    sched.write := Bool(true)
    regActualRowID(freeSlot) := entryHead.i
    regNZLeft(freeSlot) := entryHead.j
  }

  // ==========================================================================
  // main reducer datapath

  // workQ flows directly into the operand queues
  workQ.deq <> opQEnq
  opQEnqSel := opQEnq.bits.ind  // selected according to ind

  // the opArb arbiter pulls out operand pairs that are ready to go, and puts
  // them into the addQ
  opArb.out <> addQ.enq
  addQ.deq <> add.in

  // need to manually connect here due to naming incompatibilities
  resQ.enq.valid := add.out.valid
  resQ.enq.bits.value := add.out.bits.v
  resQ.enq.bits.ind := add.out.bits.i
  add.out.ready := resQ.enq.ready

  // resQ goes into resOpQ or retQ, decided by isRowFinished
  val resQHeadNZLeft = regNZLeft(resQ.deq.bits.ind) // head # ops left
  val resQHeadRowID = regActualRowID(resQ.deq.bits.ind) // head row id
  val isRowFinished = (resQHeadNZLeft <= UInt(2))

  val resQDests = Module(new DecoupledOutputDemux(p.vi, 2)).io
  resQDests.sel := isRowFinished
  resQ.deq <> resQDests.in

  resQDests.out(0) <> resOpQ.enq
  oneQ.deq <> retQArb.in(0)
  resQDests.out(1) <> retQArb.in(1)
  // resQ retire: replace internal id with real row number
  retQArb.in(1).bits.ind := resQHeadRowID

  // the retQ arbiter mixes oneQ and ready-to-retire from resQ into retQ
  retQArb.out <> retQ.enq

  // the resOpQ flows into a demuxer for joining up with the operands via the
  // upsizers
  resOpQ.deq <> upsEntry

  // remove from scheduler when all operations completed
  sched.clear_tag := resQHeadRowID
  sched.clear_hit := Bool(false)

  when(resQ.deq.valid & resQ.deq.ready) {
    when(isRowFinished) { sched.clear_hit := Bool(true) }
    sched.clear_hit := isRowFinished
    // decrement the # operations left for the head row
    resQHeadNZLeft := resQHeadNZLeft - UInt(1)
  }

  // expose results
  retQ.deq <> io.results

  // ==========================================================================
  // various sanity checks

  def printScheduler() = {
    printf("Scheduler dump: \n")
    for(i <- 0 until p.issueWindow) {
      printf("%d: valid=%d rowNum=%d nzLeft=%d \n",
        UInt(i), sched.valid_bits(i), regActualRowID(i), regNZLeft(i)
      )
    }
    printf("=============================================================\n")
  }

  // cannot enter in more than one way
  when(io.operands.ready & io.operands.valid) {
    val entWays = Cat(enterAsNewReg, enterAsNew1, enterAsExisting)
    when(PopCount(entWays) > UInt(1)) {
      printf("***ERROR! Entering in multiple ways: %b \n", entWays)
    }
  }

  // each entry should be found only once in the scheduler
  when(PopCount(sched.hits) > UInt(1)) {
    printf("***ERROR! Found multiple scheduler hits for %d\n",
      sched.tag
    )
    printScheduler()
  }

  when(resQ.deq.valid & resQ.deq.ready & isRowFinished &
    (ups(resQ.deq.bits.ind).aCount != UInt(0) ||
      ups(resQ.deq.bits.ind).bCount != UInt(0))
    ) {
    printf("***ERROR! retiring element %d still has stuff in queue\n",
      resQ.deq.bits.ind
    )
    printScheduler()
  }

  when(opQEnq.ready & opQEnq.valid & regNZLeft(opQEnq.bits.ind) < UInt(2)) {
    printf("***ERROR! This should not go to opQ: %d %d \n",
      opQEnq.bits.ind, opQEnq.bits.value
    )
    printScheduler()
  }

  when(addQ.enq.ready & addQ.enq.valid &
    regNZLeft(addQ.enq.bits.rowInd) < UInt(2)
  ) {
    printf("***ERROR! This should not go to addQ: %d \n",
      addQ.enq.bits.rowInd
    )
    printScheduler()
  }

  when(resQ.deq.valid & !sched.valid_bits(resQ.deq.bits.ind)) {
    printf("***ERROR! resQ has result from invalid slot %d val %d \n",
      resQ.deq.bits.ind, resQ.deq.bits.value
    )
    printScheduler()
  }

  // printfs for debugging ====================================================
  if(verbose_debug) {
    when(oneQ.enq.ready & oneQ.enq.valid) {
      printf("issued 1-entry for slot %d val %d\n",
        oneQ.enq.bits.ind, oneQ.enq.bits.value)
    }

    when (workQ.enq.valid & workQ.enq.ready) {
      printf("Entered workQ: row=%d id=%d value=%d \n",
        entryHead.i, workQ.enq.bits.ind, workQ.enq.bits.value)
    }

    when(opQEnq.ready & opQEnq.valid) {
      printf("add to opQ %d, value %d \n", opQEnqSel, opQEnq.bits.value)
    }

    when(addQ.enq.valid & addQ.enq.ready) {
      printf("enter addQ: id %d val1 %d val2 %d \n",
      addQ.enq.bits.rowInd, addQ.enq.bits.matrixVal, addQ.enq.bits.vectorVal)
    }

    when(resQ.deq.valid & resQ.deq.ready) {
      printf("removed from resQ, id %d NZLeft %d isFinished %d\n",
      resQ.deq.bits.ind, resQHeadNZLeft, isRowFinished)
    }

    when(retQ.enq.valid & retQ.enq.ready) {
      printf("retire: rowind %d sum %d \n", retQ.enq.bits.ind, retQ.enq.bits.value)
    }
  }
}

// produce a pair of operands for an associative op from an incoming pair of
// streams. basically a pair of output queues and a crossbar that switches
// to make sure both the output queues can get filled, regardless
// of which input streams are active.
// probably could have been much simpler...but this seems to work.
class RowMajorReducerUpsizerIO(p: SeyrekParams) extends Bundle {
  val inA = Decoupled(p.vi).flip
  val inB = Decoupled(p.vi).flip
  val out = Decoupled(p.wu)
  val aCount = UInt(OUTPUT, 2)
  val bCount = UInt(OUTPUT, 4)

  override def cloneType: this.type = new RowMajorReducerUpsizerIO(p).asInstanceOf[this.type]
}

class RowMajorReducerUpsizer(p: SeyrekParams) extends Module {
  val io = new RowMajorReducerUpsizerIO(p)

  val qiA = FPGAQueue(io.inA, 2)
  val qiB = FPGAQueue(io.inB, 8)

  val qA = Module(new FPGAQueue(p.vi, 2)).io
  val qB = Module(new FPGAQueue(p.vi, 2)).io

  io.aCount := qA.count
  io.bCount := qB.count

  val crossSel = Bool()

  when(qiA.valid & !qiB.valid & !qB.deq.valid) {crossSel := Bool(true)}
  .elsewhen(!qiA.valid & qiB.valid & !qA.deq.valid) {crossSel := Bool(true)}
  .otherwise(crossSel := Bool(false))

  val demuxA = Module(new DecoupledOutputDemux(p.vi, 2)).io
  qiA <> demuxA.in
  demuxA.sel := crossSel

  val demuxB = Module(new DecoupledOutputDemux(p.vi, 2)).io
  qiB <> demuxB.in
  demuxB.sel := crossSel

  val muxQA = Module(new DecoupledInputMux(p.vi, 2)).io
  muxQA.out <> qA.enq
  muxQA.sel := crossSel

  val muxQB = Module(new DecoupledInputMux(p.vi, 2)).io
  muxQB.out <> qB.enq
  muxQB.sel := crossSel

  demuxA.out(0) <> muxQA.in(0)
  demuxA.out(1) <> muxQB.in(1)
  demuxB.out(0) <> muxQB.in(0)
  demuxB.out(1) <> muxQA.in(1)

  // debug and  sanity check: inds from both sources should be the same
  /*TODO don't need to keep the indices, values are enough (gen as constant)*/
  /*
  when(qA.enq.valid & qA.enq.ready) {
    printf("qA enter: %d %d\n", qA.enq.bits.value, qA.enq.bits.ind)
  }

  when(qB.enq.valid & qB.enq.ready) {
    printf("qB enter: %d %d\n", qB.enq.bits.value, qB.enq.bits.ind)
  }

  when(io.out.valid & io.out.ready) {
    printf("upsizer exit: %d %d %d \n",
      io.out.bits.rowInd, io.out.bits.matrixVal, io.out.bits.vectorVal
    )
    when(qA.deq.bits.ind != qB.deq.bits.ind) {
      printf("***ERROR! addQ has different input inds: workQ = %d opQ = %d \n",
         qA.deq.bits.ind, qB.deq.bits.ind)
     }
  }
  */
  StreamJoin(
    inA = qA.deq, inB = qB.deq, genO = p.wu,
    join = {(a: ValIndPair, b: ValIndPair) => WorkUnit(a.value, b.value, a.ind)}
  ) <> io.out
}
