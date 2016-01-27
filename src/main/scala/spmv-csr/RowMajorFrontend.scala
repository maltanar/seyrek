package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsOCM._
import TidbitsStreams._

class RowMajorFrontendIO(p: SeyrekParams) extends Bundle with SeyrekCtrlStat {
  val csr = new CSRSpMV(p).asInput
  // length of each row, in-order
  val rowLen = Decoupled(p.i).flip
  // work units to the reducer
  val workUnits = Decoupled(p.wu).flip
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

  io.rowLen.ready := Bool(true)

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
  val rownums = NaturalNumbers(p.indWidth, startRegular, io.csr.rows)

  // this queue is needed to decouple the consumption rate of the backend
  // rowLen stream from the consumption rate of the reducer
  // TODO make parametrizable?
  val rowLen = FPGAQueue(io.rowLen, 16)

  StreamJoin(inA = rownums, inB = rowLen, genO = p.ii,
    join = {(a: UInt, b: UInt) => IndIndPair(a,b) }
  ) <> red.rowLen

  // push out results from reducer
  red.results <> io.results
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
  val bCount = UInt(OUTPUT, 2)

  override def cloneType: this.type = new RowMajorReducerUpsizerIO(p).asInstanceOf[this.type]
}

class RowMajorReducerUpsizer(p: SeyrekParams) extends Module {
  val io = new RowMajorReducerUpsizerIO(p)

  val qiA = FPGAQueue(io.inA, 2)
  val qiB = FPGAQueue(io.inB, 2)

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

// note that the name "RowMajorFrontend" is a slight misnomer; the design will
// support small deviations from row-major (needed to support out-of-order
//  data returns from the x read)
class RowMajorReducer(p: SeyrekParams) extends Module {
  val opQDepth = 16   /*TODO parametrize / auto-compute */
  val io = new Bundle {
    // length of each row. indA = row number, indB = num nonzeroes in row
    val rowLen = Decoupled(p.ii).flip
    // partial products in
    val operands = Decoupled(p.vi).flip
    // results out
    val results = Decoupled(p.vi)
  }

  // queues for holding operand + scheduler entry number
  val opQ = Vec.fill(p.issueWindow) {Module(new FPGAQueue(p.vi, opQDepth)).io}
  // upsizer for issuing add ops. inA from the adder, inB from the opQ
  val ups = Vec.fill(p.issueWindow) {Module(new RowMajorReducerUpsizer(p)).io}
  // demux for the upsizer stream inputs
  val upsEntrySel = UInt(width = log2Up(p.issueWindow))
  val upsEntry = DecoupledOutputDemux(upsEntrySel, ups.map(_.inA))
  upsEntrySel := upsEntry.bits.ind
  // round robin arbiter for popping the op pairs for the adder
  val opArb = Module(new RRArbiter(p.wu, p.issueWindow)).io
  for(i <- 0 until p.issueWindow) {
    opQ(i).deq <> ups(i).inB
    ups(i).out <> opArb.in(i)
  }
  // count of each op queue, useful for debug
  val counts = Vec(opQ.map(x => x.count))
  // signal for controlling the enq index to the opQ's
  val opQEnqSel = UInt(width = log2Up(p.issueWindow))
  // demuxer for enq into op queues
  val opQEnq = DecoupledOutputDemux(opQEnqSel, opQ.map(x => x.enq))

  // the adder as defined by the semiring
  val add = Module(new ContextfulSemiringOp(p, p.makeSemiringAdd)).io
  // queue that keeps the operands with translated IDs
  val workQ = Module(new FPGAQueue(p.vi, 2)).io
  val addQ = Module(new FPGAQueue(p.wu, 2)).io  // adder inputs
  val resQ = Module(new FPGAQueue(p.vi, 2)).io  // adder results
  val resOpQ = Module(new FPGAQueue(p.vi, 2)).io  // adder results back to ops
  val retQ = Module(new FPGAQueue(p.vi, 2)).io  // rows ready to retire

  // rr arbiter for mixing 1- and 0-length results into resQ
  val resQArb = Module(new RRArbiter(p.vi, 3)).io
  val zeroQ = Module(new FPGAQueue(p.vi, 2)).io // injected zeroes
  val oneQ = Module(new FPGAQueue(p.vi, 2)).io  // injected ones

  // scheduler bookkeeping
  // the row major reducer uses a scheduler to keep track of the status of each
  // row: may be several rows in the reducer at a time due to out-of-order
  val sched = Module(new CAM(entries = p.issueWindow, tag_bits = p.indWidth)).io
  val regNZLeft = Vec.fill(p.issueWindow) { Reg(init = UInt(0, p.indWidth)) }
  val regActualRowID = Vec.fill(p.issueWindow) { Reg(init = UInt(0, p.indWidth)) }

  // ******************** reducer logic ************************


  // entry into scheduler: new rowLen becomes available
  val freeSlot = sched.freeInd
  // check both against scheduler and zeroQ
  io.rowLen.ready := sched.hasFree & zeroQ.enq.ready
  // use the row index as the CAM tag
  sched.write_tag := io.rowLen.bits.indA
  sched.write := Bool(false)

  // emit a zero if this row had no nonzero elements
  val isZeroRow = io.rowLen.bits.indB === UInt(0)
  zeroQ.enq.valid := isZeroRow & io.rowLen.valid & sched.hasFree
  zeroQ.enq.bits.value := UInt(0) // TODO semiring zero
  zeroQ.enq.bits.ind := freeSlot  // zeroQ elem will be generated on sched ins.

  when(io.rowLen.ready & io.rowLen.valid) {
    sched.write := Bool(true) // add entry into scheduler
    regNZLeft(freeSlot) := io.rowLen.bits.indB // # nonzeroes left
    // TODO duplicate this regNZLeft to control both ins and outs
    // also save rowid in register for easy access
    regActualRowID(freeSlot) := io.rowLen.bits.indA
  }

  // new op for existing scheduler entry received
  // - check if exists in scheduler
  // - if so, translate row number into scheduler entry number and put in redQ
  // - special case: put into oneQ if this was a one-element row

  sched.tag := io.operands.bits.ind
  val schedEntryNum = PriorityEncoder(sched.hits) // entry where rowid is found
  val isRowSingleElem = regNZLeft(schedEntryNum) === UInt(1)
  val targetReady = (isRowSingleElem & oneQ.enq.ready)  | (!isRowSingleElem & workQ.enq.ready)
  io.operands.ready := sched.hit & targetReady

  // enqueue operand into oneQ if this is a single-element row
  oneQ.enq.valid := io.operands.valid & sched.hit & isRowSingleElem
  oneQ.enq.bits.ind := schedEntryNum  // translate row number into scheduler entry number
  oneQ.enq.bits.value := io.operands.bits.value

  // enqueue operand into workQ for all else
  workQ.enq.valid := io.operands.valid & sched.hit & !isRowSingleElem
  workQ.enq.bits.ind := schedEntryNum // translate row number into scheduler entry number
  workQ.enq.bits.value := io.operands.bits.value

  // TODO check number of operands entering for sanity/error checking?

  // workQ flows directly into the operand queues
  workQ.deq <> opQEnq
  opQEnqSel := opQEnq.bits.ind  // selected according to ind

  // the opArb arbiter pulls out operand pairs that are ready to go, and puts
  // them into the addQ
  opArb.out <> addQ.enq
  addQ.deq <> add.in

  // the resQ arbiter creates a mix between the zero-length, one-length and add
  // result queues
  zeroQ.deq <> resQArb.in(0)
  oneQ.deq <> resQArb.in(1)
  add.out <> resQArb.in(2)
  resQArb.out <> resQ.enq

  // resQ goes into resOpQ or retQ, decided by isRowFinished
  val resQHeadNZLeft = regNZLeft(resQ.deq.bits.ind) // head # ops left
  val resQHeadRowID = regActualRowID(resQ.deq.bits.ind) // head row id
  val isRowFinished = (resQHeadNZLeft <= UInt(2))

  val resQDests = Module(new DecoupledOutputDemux(p.vi, 2)).io
  resQDests.sel := isRowFinished
  resQ.deq <> resQDests.in

  resQDests.out(0) <> resOpQ.enq
  resQDests.out(1) <> retQ.enq
  retQ.enq.bits.ind := resQHeadRowID

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

  // sanity check: each entry should be found only once in the scheduler
  when(PopCount(sched.hits) > UInt(1)) {
    printf("***ERROR! Found multiple scheduler hits for %d\n",
      sched.tag
    )
  }
  // other sanity checks
  when(resQ.deq.valid & resQ.deq.ready & isRowFinished &
    (ups(resQ.deq.bits.ind).aCount != UInt(0) ||
      ups(resQ.deq.bits.ind).bCount != UInt(0))
    ) {
    printf("***ERROR! retiring element %d still has stuff in queue\n",
      resQ.deq.bits.ind
    )
  }

  when(workQ.enq.ready & workQ.enq.valid &
    regNZLeft(workQ.enq.bits.ind) < UInt(2)
  ) {
    printf("***ERROR! This should not go to workQ: %d %d \n",
      workQ.enq.bits.ind, workQ.enq.bits.value
    )
  }

  when(opQEnq.ready & opQEnq.valid & regNZLeft(opQEnq.bits.ind) < UInt(2)) {
    printf("***ERROR! This should not go to opQ: %d %d \n",
      opQEnq.bits.ind, opQEnq.bits.value
    )
  }

  when(addQ.enq.ready & addQ.enq.valid &
    regNZLeft(addQ.enq.bits.rowInd) < UInt(2)
  ) {
    printf("***ERROR! This should not go to addQ: %d \n",
      addQ.enq.bits.rowInd
    )
  }

  when(resQ.deq.valid & !sched.valid_bits(resQ.deq.bits.ind)) {
    printf("***ERROR! resQ has result from invalid slot %d val %d \n",
      resQ.deq.bits.ind, resQ.deq.bits.value
    )
  }

  // printfs for debugging ====================================================
  val verbose_debug = false
  if(verbose_debug) {
    printf("====================================================\n")
    printf("queue counts: wq = %d addq = %d resq = %d retq = %d resOpQ = %d zeroQ = %d oneQ = %d \n",
      workQ.count, addQ.count, resQ.count, retQ.count, resOpQ.count, zeroQ.count, oneQ.count
    )
    printf("opQ counts: \n")
    for(i <- 0 until p.issueWindow) {printf("%d ", counts(i))}
    printf("\n")

    printf("ups counts: \n")
    for(i <- 0 until p.issueWindow) {printf("%d %d : ", ups(i).aCount, ups(i).bCount )}
    printf("\n")

    printf("scheduler valid bits: \n")
    for(i <- 0 until p.issueWindow) {printf("%d ", sched.valid_bits(i))}
    printf("\n")

    printf("rowID for each slot: \n")
    for(i <- 0 until p.issueWindow) {printf("%d ", regActualRowID(i))}
    printf("\n")

    printf("nz counts (may be unoccupied): \n")
    for(i <- 0 until p.issueWindow) {printf("%d ", regNZLeft(i))}
    printf("\n")

    when(io.rowLen.ready & io.rowLen.valid) {
      printf("scheduler add: slot = %d rowid = %d rowlen = %d\n",
        freeSlot, io.rowLen.bits.indA, io.rowLen.bits.indB)
    }

    when(zeroQ.enq.ready & zeroQ.enq.valid) {
      printf("issued 0-entry for slot %d\n", zeroQ.enq.bits.ind)
    }

    when(oneQ.enq.ready & oneQ.enq.valid) {
      printf("issued 1-entry for slot %d val %d\n",
        oneQ.enq.bits.ind, oneQ.enq.bits.value)
    }

    when(io.operands.valid & !sched.hit) {
      printf("op from row %d can't enter, not in sched\n", io.operands.bits.ind)
    } .elsewhen (workQ.enq.valid & workQ.enq.ready) {
      printf("entered workQ: id %d value %d \n", schedEntryNum, io.operands.bits.value)
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
