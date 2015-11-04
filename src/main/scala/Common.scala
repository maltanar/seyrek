package Seyrek

import Chisel._
import TidbitsDMA._

trait SeyrekParams {
  val memPtrWidth: Int = 64 // TODO take from mrp
  def indWidth: Int
  def valWidth: Int
  def mrp: MemReqParams
  // context memory creation
  def makeContextMemory: () => ContextMem
  // semiring operations
  def makeSemiringAdd: () => SemiringOp
  def makeSemiringMul: () => SemiringOp
  // scheduler-related
  def issueWindow: Int
  def makeScheduler: () => Scheduler
  // type definitions for convenience, useful as clone types
  val v = UInt(width = valWidth)
  val i = UInt(width = indWidth)
  val wu = new WorkUnit(valWidth, indWidth)
  val vi = new ValIndPair(valWidth, indWidth)
}

class WorkUnit(valWidth: Int, indWidth: Int) extends Bundle {
  val matrixVal = UInt(width = valWidth)
  val vectorVal = UInt(width = valWidth)
  val rowInd = UInt(width = indWidth)
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
}

object ValIndPair {
  def apply(v: UInt, i: UInt): ValIndPair = {
    val vip = new ValIndPair(v.getWidth(), i.getWidth())
    vip.value := v
    vip.ind := i
    vip
  }
}

class CSCSpMV(p: SeyrekParams) extends Bundle {
  val colPtr = UInt(width = p.memPtrWidth)
  val rowInd = UInt(width = p.memPtrWidth)
  val nzData = UInt(width = p.memPtrWidth)
  val inpVec = UInt(width = p.memPtrWidth)
  val outVec = UInt(width = p.memPtrWidth)

  val rows = UInt(width = p.indWidth)
  val cols = UInt(width = p.indWidth)
  val nz = UInt(width = p.indWidth)
}

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
