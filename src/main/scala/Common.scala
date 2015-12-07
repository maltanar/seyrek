package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsOCM._
import TidbitsStreams._
import TidbitsMath._

// TODO sanity-check whether scheduler and contextmem orderings match
// TODO add ordering params to semirings as well?

trait SeyrekParams {
  def accelName: String
  def numPEs: Int       // number of processing elements (PEs)
  def portsPerPE: Int   // number of memory ports per PE
  def indWidth: Int     // bitwidth of CSC indices (colptr, rowind)
  def valWidth: Int     // bitwidth of matrix/vector values
  val ptrWidth: Int = 64  // large enough for big&small platforms
  def mrp: MemReqParams
  // context memory creation
  def makeContextMemory: ReadChanParams => ContextMem
  // semiring operations
  def makeSemiringAdd: () => BinaryMathOp
  def makeSemiringMul: () => BinaryMathOp
  // scheduler-related
  def issueWindow: Int
  def makeScheduler: () => Scheduler
  // type definitions for convenience, useful as clone types
  def v = UInt(width = valWidth)  // values
  def i = UInt(width = indWidth)  // index (context identifier / row index)
  def wu = new WorkUnit(valWidth, indWidth) // (value, value, index)
  def vi = new ValIndPair(valWidth, indWidth) // (value, index)
  def vv = new BinaryMathOperands(valWidth) // (value, value)
  // channel-to-port mapping
   def chanConfig: Map[String, ReadChanParams]
}

class WorkUnit(valWidth: Int, indWidth: Int) extends Bundle {
  val matrixVal = UInt(width = valWidth)
  val vectorVal = UInt(width = valWidth)
  val rowInd = UInt(width = indWidth)

  override def cloneType: this.type = new WorkUnit(valWidth, indWidth).asInstanceOf[this.type]
}

object WorkUnit {
  def apply(m: UInt, v: UInt, i: UInt): WorkUnit = {
    val workUnit = new WorkUnit(v.getWidth(), i.getWidth())
    workUnit.matrixVal := m
    workUnit.vectorVal := v
    workUnit.rowInd := i
    workUnit
  }
}

class ValIndPair(valWidth: Int, indWidth: Int) extends Bundle {
  val value = UInt(width = valWidth)
  val ind = UInt(width = indWidth)

  override def cloneType: this.type = new ValIndPair(valWidth, indWidth).asInstanceOf[this.type]
}

object ValIndPair {
  def apply(v: UInt, i: UInt): ValIndPair = {
    val vip = new ValIndPair(v.getWidth(), i.getWidth())
    vip.value := v
    vip.ind := i
    vip
  }
}

// "contextful" version of semiring op (also carries the index through the
// pipeline through a FIFO queue, capacity = latency)

class ContextfulSemiringOpIO(p: SeyrekParams) extends Bundle {
  val in = Decoupled(p.wu).flip
  val out = Decoupled(p.vi)
}

class ContextfulSemiringOp(p: SeyrekParams, instFxn: () => BinaryMathOp)
extends Module {
  val io = new ContextfulSemiringOpIO(p)
  val opInst = Module(instFxn())
  val forker = Module(new StreamFork(genIn = p.wu, genA = p.vv, genB = p.i,
    forkA = {wu: WorkUnit => BinaryMathOperands(wu.matrixVal, wu.vectorVal)},
    forkB = {wu: WorkUnit => wu.rowInd}
  )).io
  val joiner = Module(new StreamJoin(genA = p.v, genB = p.i, genOut = p.vi,
    join = {(v: UInt, i: UInt) => ValIndPair(v, i)}
  )).io

  io.in <> forker.in
  forker.outA <> opInst.io.in
  opInst.io.out <> joiner.inA
  joiner.out <> io.out


  if(opInst.latency == 0) {
    forker.outB <> joiner.inB
  } else {
    // queue should have at least two elements to guarantee full throughout
    // could use smaller sizes with pipe/flow, but this gives better timing
    val qCap = if(opInst.latency < 2) 2 else opInst.latency
    val indQ = Module(new FPGAQueue(p.i, qCap)).io
    forker.outB <> indQ.enq
    indQ.deq <> joiner.inB
  }
}

// bundles and types used for interfaces and control/status

class CSCSpMV(p: SeyrekParams) extends Bundle {
  val colPtr = UInt(width = p.ptrWidth)
  val rowInd = UInt(width = p.ptrWidth)
  val nzData = UInt(width = p.ptrWidth)
  val inpVec = UInt(width = p.ptrWidth)
  val outVec = UInt(width = p.ptrWidth)

  val rows = UInt(width = p.indWidth)
  val cols = UInt(width = p.indWidth)
  val nz = UInt(width = p.indWidth)
}

// TODO add error signaling here and throughout the framework
trait SeyrekCtrlStat {
  val start = Bool(INPUT)
  val finished = Bool(OUTPUT)
  val mode = UInt(INPUT, width = 10)
}

object SeyrekModes {
  val START_REGULAR = UInt(0, 10)
  val START_INIT = UInt(1, 10)
  val START_FLUSH = UInt(2, 10)
}
