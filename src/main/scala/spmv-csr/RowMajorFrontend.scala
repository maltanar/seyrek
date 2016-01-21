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

  StreamJoin(inA = rownums, inB = io.rowLen, genO = p.ii,
    join = {(a: UInt, b: UInt) => IndIndPair(a,b) }
  ) <> red.rowLen

  // push out results from reducer
  red.results <> io.results
}

// produce a pair of operands for an associative op from an incoming pair of
// streams. basically a pair of output queues and a crossbar that switches
// every cycle to make sure both the output queues can get filled, regardless
// of which input streams are active.
// probably could have been much simpler...
class RowMajorReducerUpsizer(p: SeyrekParams) extends Module {
  val io = new Bundle {
    val inA = Decoupled(p.vi).flip
    val inB = Decoupled(p.vi).flip
    val out = Decoupled(p.wu)
  }
  val qA = Module(new FPGAQueue(p.vi, 2)).io
  val qB = Module(new FPGAQueue(p.vi, 2)).io



  val regCrossSel = Reg(init = Bool(false))
  val txnA = qA.enq.valid & qA.enq.ready
  val txnB = qB.enq.valid & qB.enq.ready

  // switch the crossing every odd # of transactions
  when(txnA ^ txnB) {regCrossSel := !regCrossSel}

  val demuxA = Module(new DecoupledOutputDemux(p.vi, 2)).io
  io.inA <> demuxA.in
  demuxA.sel := regCrossSel

  val demuxB = Module(new DecoupledOutputDemux(p.vi, 2)).io
  io.inB <> demuxB.in
  demuxB.sel := regCrossSel

  val muxQA = Module(new DecoupledInputMux(p.vi, 2)).io
  muxQA.out <> qA.enq
  muxQA.sel := regCrossSel

  val muxQB = Module(new DecoupledInputMux(p.vi, 2)).io
  muxQB.out <> qB.enq
  muxQB.sel := regCrossSel

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

  // scheduler bookkeeping
  // the row major reducer uses a scheduler to keep track of the status of each
  // row: may be several rows in the reducer at a time due to out-of-order
  val sched = Module(new CAM(entries = p.issueWindow, tag_bits = p.indWidth)).io
  val regNZLeft = Vec.fill(p.issueWindow) { Reg(init = UInt(0, p.indWidth)) }
  val regActualRowID = Vec.fill(p.issueWindow) { Reg(init = UInt(0, p.indWidth)) }

  // ******************** reducer logic ************************

  // entry into scheduler: new rowLen becomes available
  val freeSlot = sched.freeInd
  io.rowLen.ready := sched.hasFree
  // use the row index as the CAM tag
  sched.write_tag := io.rowLen.bits.indA
  sched.write := Bool(false)

  when(io.rowLen.ready & io.rowLen.valid) {
    sched.write := Bool(true) // add entry into scheduler
    regNZLeft(freeSlot) := io.rowLen.bits.indB // # nonzeroes left
    // TODO duplicate this regNZLeft to control both ins and outs
    // also save rowid in register for easy access
    regActualRowID(freeSlot) := io.rowLen.bits.indA

    // TODO handle zero- and one-length ops!
    when(io.rowLen.bits.indB <= UInt(1)) {
      printf("ERROR: CSR reducer does not yet support 1- and 0-len rows!\n")
    }
  }

  // new op for existing scheduler entry received
  // - check if exists in scheduler
  // - if so, translate row number into scheduler entry number and put in redQ
  sched.tag := io.operands.bits.ind
  io.operands.ready := sched.hit & workQ.enq.ready
  val schedEntryNum = PriorityEncoder(sched.hits) // entry where rowid is found

  workQ.enq.valid := io.operands.valid & sched.hit
  workQ.enq.bits.ind := schedEntryNum // translate row number into scheduler entry number
  workQ.enq.bits.value := io.operands.bits.value

  // TODO check number of operands entering

  // workQ flows directly into the operand queues
  workQ.deq <> opQEnq
  opQEnqSel := opQEnq.bits.ind  // selected according to ind

  // the opArb arbiter pulls out operand pairs that are ready to go, and puts
  // them into the addQ
  opArb.out <> addQ.enq
  addQ.deq <> add.in
  add.out <> resQ.enq // all adder results flow into resQ

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
    sched.clear_hit := isRowFinished
    // decrement the # operations left for the head row
    resQHeadNZLeft := resQHeadNZLeft - UInt(1)
  }

  // expose results
  retQ.deq <> io.results

  // printfs for debugging ====================================================
  val verbose_debug = false
  if(verbose_debug) {
    printf("====================================================\n")
    printf("queue counts: wq = %d addq = %d resq = %d retq = %d\n",
      workQ.count, addQ.count, resQ.count, retQ.count
    )
    printf("opQ counts: \n")
    for(i <- 0 until p.issueWindow) {printf("%d ", counts(i))}
    printf("\n")

    when(io.rowLen.ready & io.rowLen.valid) {
      printf("scheduler add: slot = %d rowid = %d rowlen = %d\n",
        freeSlot, io.rowLen.bits.indA, io.rowLen.bits.indB)
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

/*
class RMReducerTester(c: RowMajorReducer) extends Tester(c) {
  poke(c.io.results.ready, 1)

  poke(c.io.operands.bits.value, 10)
  poke(c.io.operands.bits.ind, 1)
  poke(c.io.operands.valid, 1)
  peek(c.io.operands.ready)

  poke(c.io.rowLen.bits.indA, 1)
  poke(c.io.rowLen.bits.indB, 2)
  poke(c.io.rowLen.valid, 1)
  peek(c.io.rowLen.ready)
  step(1)
  peek(c.io.operands.ready)
  poke(c.io.rowLen.bits.indA, 2)
  poke(c.io.rowLen.bits.indB, 4)
  peek(c.io.rowLen.ready)
  step(1)
  poke(c.io.rowLen.valid, 0)
  step(1)
  peek(c.io.rowLen.valid)
  poke(c.io.operands.valid, 0)

  for(i <- 0 until 10)
    step(1)
}
*/
