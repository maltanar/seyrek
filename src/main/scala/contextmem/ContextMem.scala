package Seyrek

import Chisel._
import TidbitsDMA._

class ContextMemParams(
  val idBits: Int,
  val dataBits: Int,
  val chanID: Int, // TODO permit a range of IDs instead, or let backend assign
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

abstract class ContextMem(val p: ContextMemParams) extends Module {
  val io = new ContextMemIO(p)

  // whether the ContextMem responds to load/save commands in-order
  def inOrder: Boolean

  // useful printfs to debug ContextMem, uncomment as needed
  /*
  when(io.contextLoadReq.valid & io.contextLoadReq.ready) {
    printf("contextLoadReq: id = %d ctx = %x\n", io.contextLoadReq.bits.ind,
    io.contextLoadReq.bits.value)
  }

  when(io.contextSaveReq.valid & io.contextSaveReq.ready) {
    printf("contextSaveReq: id = %d ctx = %x\n", io.contextSaveReq.bits.ind,
    io.contextSaveReq.bits.value)
  }

  when(io.contextLoadRsp.valid & io.contextLoadRsp.ready) {
    printf("contextLoadRsp: id = %d ctx = %x %x\n",
       io.contextLoadRsp.bits.rowInd, io.contextLoadRsp.bits.matrixVal,
       io.contextLoadRsp.bits.vectorVal
     )
  }

  when(io.contextSaveRsp.valid & io.contextSaveRsp.ready) {
    printf("contextSaveRsp: id = %d \n", io.contextSaveRsp.bits)
  }
  */
}
