package Seyrek

import Chisel._
import TidbitsDMA._

class ContextMemParams(
  val idBits: Int,
  val dataBits: Int,
  val mrp: MemReqParams
)

class ContextMemIO(p: ContextMemParams) extends Bundle with SeyrekCtrlStat {
  // SeyrekCtrlStat is inherited from base trait
  // context load port
  val contextLoadReq = Decoupled(new ValIndPair(p.dataBits, p.idBits)).flip
  val contextLoadRsp = Decoupled(new WorkUnit(p.dataBits, p.idBits))
  // context save port
  val contextSaveReq = Decoupled(new ValIndPair(p.dataBits, p.idBits)).flip
  val contextSaveRsp = Decoupled(UInt(width = p.idBits))
  // main memory access port
  val mainMem = new GenericMemoryMasterPort(p.mrp)
  val contextBase = UInt(INPUT, width = p.mrp.addrWidth)
}

// base abstract class for context storage memories

abstract class ContextMem(p: ContextMemParams) extends Module {
  val io = new ContextMemIO(p)

  // whether the ContextMem responds to load/save commands in-order
  def inOrder: Boolean
}
