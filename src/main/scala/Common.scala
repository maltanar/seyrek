package Seyrek

import Chisel._
import fpgatidbits.dma._
import fpgatidbits.ocm._
import fpgatidbits.streams._
import fpgatidbits.math._

trait SeyrekParams {
  def accelName: String
  def numPEs: Int       // number of processing elements (PEs)
  def portsPerPE: Int   // number of memory ports per PE
  def indWidth: Int     // bitwidth of CSC indices (colptr, rowind)
  def valWidth: Int     // bitwidth of matrix/vector values
  val ptrWidth: Int = 64  // large enough for big&small platforms
  def mrp: MemReqParams
  // semiring operations
  def makeSemiringAdd: () => BinaryMathOp
  def makeSemiringMul: () => BinaryMathOp
  // scheduler-related
  def issueWindow: Int
  // type definitions for convenience, useful as clone types
  def v = UInt(width = valWidth)  // values
  def i = UInt(width = indWidth)  // index (context identifier / row index)
  def wu = new WorkUnit(valWidth, indWidth) // (value, value, index)
  def wul = new WorkUnitAndLen(valWidth, indWidth) // work unit plus row len
  def vi = new ValIndPair(valWidth, indWidth) // (value, index)
  def vv = new BinaryMathOperands(valWidth) // (value, value)
  def ii = new IndIndPair(indWidth) // (index, index)
  def vii = new ValIndInd(valWidth, indWidth) // (value, index, index)
  def viil = new ValIndIndLen(valWidth, indWidth) // ValIndInd and row len
  // channel-to-port mapping
   def chanConfig: Map[String, ReadChanParams]
}

class ValIndInd(valWidth: Int, indWidth: Int) extends Bundle {
  val v = UInt(width = valWidth)
  val i = UInt(width = indWidth)
  val j = UInt(width = indWidth)

  override def cloneType: this.type = new ValIndInd(valWidth, indWidth).asInstanceOf[this.type]
}

object ValIndInd {
  def apply(v: UInt, i: UInt, j: UInt): ValIndInd = {
    val vii = new ValIndInd(v.getWidth(), i.getWidth())
    vii.v := v
    vii.i := i
    vii.j := j
    vii
  }
}

class ValIndIndLen(valWidth: Int, indWidth: Int) extends ValIndInd(valWidth, indWidth) {
  val rl = UInt(width = indWidth)

  override def cloneType: this.type = new ValIndIndLen(valWidth, indWidth).asInstanceOf[this.type]
}

object ValIndIndLen {
  def apply(v: UInt, i: UInt, j: UInt, rl: UInt): ValIndIndLen = {
    val viil = new ValIndIndLen(v.getWidth(), i.getWidth())
    viil.v := v
    viil.i := i
    viil.j := j
    viil.rl := rl
    viil
  }
}

class WorkUnit(valWidth: Int, indWidth: Int) extends Bundle {
  val matrixVal = UInt(width = valWidth)
  val vectorVal = UInt(width = valWidth)
  val rowInd = UInt(width = indWidth)

  override def cloneType: this.type = new WorkUnit(valWidth, indWidth).asInstanceOf[this.type]
}

class WorkUnitAndLen(valWidth: Int, indWidth: Int) extends WorkUnit(valWidth, indWidth) {
  val rowLen = UInt(width = indWidth)

  override def cloneType: this.type = new WorkUnitAndLen(valWidth, indWidth).asInstanceOf[this.type]
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

object WorkUnitAndLen {
  def apply(m: UInt, v: UInt, i: UInt, rl: UInt): WorkUnitAndLen = {
    val workUnit = new WorkUnitAndLen(v.getWidth(), i.getWidth())
    workUnit.matrixVal := m
    workUnit.vectorVal := v
    workUnit.rowInd := i
    workUnit.rowLen := rl
    workUnit
  }
}

class IndIndPair(indWidth: Int) extends Bundle {
  val indA = UInt(width = indWidth)
  val indB = UInt(width = indWidth)

  override def cloneType: this.type = new IndIndPair(indWidth).asInstanceOf[this.type]
}

object IndIndPair {
  def apply(ia: UInt, ib: UInt): IndIndPair = {
    val iip = new IndIndPair(ia.getWidth())
    iip.indA := ia
    iip.indB := ib
    iip
  }
}

class ValIndPair(valWidth: Int, indWidth: Int) extends PrintableBundle {
  val value = UInt(width = valWidth)
  val ind = UInt(width = indWidth)

  val printfStr = "(v = %d, i = %d)\n"
  val printfElems = {() => Seq(value, ind)}

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
  val in = Decoupled(p.wul).flip
  val out = Decoupled(p.vii)
}

class ContextfulSemiringOp(p: SeyrekParams, instFxn: () => BinaryMathOp)
extends Module {
  val io = new ContextfulSemiringOpIO(p)
  val opInst = Module(instFxn())
  val forker = Module(new StreamFork(genIn = p.wul, genA = p.vv, genB = p.ii,
    forkA = {wu: WorkUnitAndLen => BinaryMathOperands(wu.matrixVal, wu.vectorVal)},
    forkB = {wu: WorkUnitAndLen => IndIndPair(wu.rowInd, wu.rowLen)}
  )).io
  val joiner = Module(new StreamJoin(genA = p.v, genB = p.ii, genOut = p.vii,
    join = {(v: UInt, ij: IndIndPair) => ValIndInd(v, ij.indA, ij.indB)}
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
    // WEIRD: observed throughput issues without the constant +2, so keeping
    // it like this for the time being
    val qCap = opInst.latency + 2
    val indQ = Module(new FPGAQueue(p.ii, qCap)).io
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

class CSRSpMV(p: SeyrekParams) extends Bundle {
  val rowPtr = UInt(width = p.ptrWidth)
  val colInd = UInt(width = p.ptrWidth)
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
  val START_CONFIG = UInt(3, 10)
}
