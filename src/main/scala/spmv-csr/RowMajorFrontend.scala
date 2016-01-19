package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsOCM._
import TidbitsStreams._


class RowMajorFrontendIO(p: SeyrekParams) extends Bundle with SeyrekCtrlStat {
  // number of rows in the matrix
  val rows = UInt(INPUT, width = p.indWidth)
  // pointer to base of output vector
  val outVec = UInt(INPUT, width = p.indWidth)
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
  val rownums = NaturalNumbers(p.indWidth, startRegular, io.rows)

  StreamJoin(inA = rownums, inB = io.rowLen, genO = p.ii,
    join = {(a: UInt, b: UInt) => IndIndPair(a,b) }
  ) <> red.rowLen

  // push out results from reducer
  red.results <> io.results
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
  val counts = Vec(opQ.map(x => x.count))
  // signals for controlling the enq/deq value
  val opQEnqSel = UInt(width = log2Up(p.issueWindow))
  val opQDeqSel = UInt(width = log2Up(p.issueWindow))
  // demuxer for enq into op queues
  val opQEnq = DecoupledOutputDemux(opQEnqSel, opQ.map(x => x.enq))
  // muxer for deq from op queues
  val opQDeq = DecoupledInputMux(opQDeqSel, opQ.map(x => x.deq))
  // queue that keeps the operands with translated IDs
  val workQ = Module(new FPGAQueue(p.vi, 2)).io
  // interleave elements to opQ with a round-robin arbiter (comes either from
  // resQ or workQ)
  val makeOpQMix = Module(new RRArbiter(p.vi, 2)).io

  // the adder as defined by the semiring
  val add = Module(new ContextfulSemiringOp(p, p.makeSemiringAdd)).io
  val addQ = Module(new FPGAQueue(p.wu, 2)).io  // adder inputs
  val resQ = Module(new FPGAQueue(p.vi, 2)).io  // adder results
  val retQ = Module(new FPGAQueue(p.vi, 2)).io  // rows ready to retire

  // scheduler bookkeeping
  // the row major reducer uses a scheduler to keep track of the status of each
  // row: may be several rows in the reducer at a time due to out-of-order
  val sched = Module(new CAM(entries = p.issueWindow, tag_bits = p.indWidth)).io
  val regOpsLeft = Vec.fill(p.issueWindow) { Reg(init = UInt(0, p.indWidth)) }
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
    regOpsLeft(freeSlot) := io.rowLen.bits.indB - UInt(1) // num ops left
    // TODO duplicate this regOpsLeft to control both ins and outs
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

  // drain the reducer work queue; send either to opQ or to addQ:
  // check if the opQ corresponding to head item's scheduler entry has items
  // - if opQ has items, send head item and head of opQ to adder
  // - if not, send head item to opQ
  val headCanOperate = (counts(workQ.deq.bits.ind) != UInt(0))

  val workQDests = Module(new DecoupledOutputDemux(p.vi, 2)).io
  workQ.deq <> workQDests.in
  workQDests.sel := headCanOperate
  workQDests.out(0) <> makeOpQMix.in(0)
  // workQDests.out(1) is connected to the adder input queue

  opQDeqSel := workQDests.out(1).bits.ind

  makeOpQMix.out <> opQEnq
  opQEnqSel := opQEnq.bits.ind

  // use StreamJoin to driver the input to the adder queue
  StreamJoin(
    inA = workQDests.out(1), inB = opQDeq, genO = p.wu, join = {
      (a: ValIndPair, b: ValIndPair) => WorkUnit(a.value, b.value, a.ind)
    }
  ) <> addQ.enq

  // sanity check: inds from both sources should be the same

  when(addQ.enq.valid & addQ.enq.ready &
    (workQDests.out(1).bits.ind != opQDeq.bits.ind)) {
    printf("***ERROR! addQ has different input inds: workQ = %d opQ = %d \n",
       workQDests.out(1).bits.ind, opQDeq.bits.ind)
  }

  addQ.deq <> add.in
  add.out <> resQ.enq

  // resQ goes into opQ or retQ, decided by isRowFinished
  val resQHeadOpsLeft = regOpsLeft(resQ.deq.bits.ind) // head # ops left
  val resQHeadRowID = regActualRowID(resQ.deq.bits.ind) // head row id
  val isRowFinished = (resQHeadOpsLeft === UInt(1))

  val resQDests = Module(new DecoupledOutputDemux(p.vi, 2)).io
  resQDests.sel := isRowFinished
  resQ.deq <> resQDests.in

  resQDests.out(0) <> makeOpQMix.in(1)
  resQDests.out(1) <> retQ.enq
  retQ.enq.bits.ind := resQHeadRowID

  // remove from scheduler when all operations completed
  sched.clear_tag := resQHeadRowID
  sched.clear_hit := Bool(false)

  when(resQ.deq.valid & resQ.deq.ready) {
    sched.clear_hit := isRowFinished
    // decrement the # operations left for the head row
    resQHeadOpsLeft := resQHeadOpsLeft - UInt(1)
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
      printf("removed from resQ, id %d opsLeft %d isFinished %d\n",
      resQ.deq.bits.ind, resQHeadOpsLeft, isRowFinished)
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
