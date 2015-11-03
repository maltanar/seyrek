package Seyrek

import Chisel._
import TidbitsDMA._

trait SeyrekParams {
  val memPtrWidth: Int = 64 // TODO take from mrp
  def indWidth: Int
  def valWidth: Int
  def mrp: MemReqParams
}

class WorkUnit(p: SeyrekParams) extends Bundle {
  val matrixVal = UInt(width = p.valWidth)
  val vectorVal = UInt(width = p.valWidth)
  val rowInd = UInt(width = p.indWidth)
}

object WorkUnit {
  def apply(m: UInt, v: UInt, i: UInt, p: SeyrekParams): WorkUnit = {
    val workUnit = new WorkUnit(p)
    workUnit.matrixVal := m
    workUnit.vectorVal := v
    workUnit.rowInd := i
    workUnit
  }
}

class ValIndPair(p: SeyrekParams) extends Bundle {
  val value = UInt(width = p.valWidth)
  val ind = UInt(width = p.indWidth)
}

object ValIndPair {
  def apply(v: UInt, i: UInt, p: SeyrekParams): ValIndPair = {
    val vip = new ValIndPair(p)
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
  val mode = UInt(width = 10)
}

object SeyrekModes {
  val START_REGULAR = UInt(0, 10)
  val START_INIT = UInt(1, 10)
  val START_FLUSH = UInt(2, 10)
}
