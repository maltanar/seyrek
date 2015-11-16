package Seyrek

import Chisel._
import TidbitsPlatformWrapper._

// simple setup for testing the init/flush modes in BRAMContextMem

class TestBRAMContextMem(p: PlatformWrapperParams) extends GenericAccelerator(p) {
  val numMemPorts = 1
  val io = new GenericAcceleratorIF(numMemPorts, p) with SeyrekCtrlStat {
    val contextBase = UInt(INPUT, 64)
  }
  io.signature := makeDefaultSignature()

  val inst = Module(new BRAMContextMem(new BRAMContextMemParams(
    depth = 1024, readLatency = 1, writeLatency = 1,
    idBits = 10, dataBits = 32, mrp = p.toMemReqParams(), chanID = 0
  ))).io

  inst.mainMem <> io.memPort(0)
  inst.contextLoadReq.valid := Bool(false)
  inst.contextLoadRsp.ready := Bool(false)
  inst.contextSaveReq.valid := Bool(false)
  inst.contextSaveRsp.ready := Bool(false)

  inst.contextBase := io.contextBase
  inst <> io
}
