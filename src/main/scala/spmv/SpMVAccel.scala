package Seyrek

import Chisel._
import TidbitsPlatformWrapper._

trait SpMVAccelParams extends PlatformWrapperParams with SeyrekParams

class SpMVAccel(p: PlatformWrapperParams) extends GenericAccelerator(p) {
  val numMemPorts = 1 // TODO should be made configurable
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val start = Bool(INPUT)
    val mode = UInt(INPUT, width = 10)
    val finished = Bool(OUTPUT)
    val csc = new CSCSpMV(p).asInput
  }
  io.signature := makeDefaultSignature()

  val backend = Module(new SpMVBackend(p))
  val frontend = Module(new SpMVFrontend(p))
  val contextmem = Module(p.makeContextMemory())

  // TODO expose more detailed status and statistics


  // TODO more flexible config for connecting mem ports
  // TODO interleave contextmem-backend reqs
  io.csc <> backend.io.csc
  backend.io.reqSeq.memRdReq <> io.memPort(0).memRdReq
  io.memPort(0).memRdRsp <> backend.io.reqSeq.memRdRsp

  io.csc <> frontend.io.csc
  backend.io.workUnits <> frontend.io.workUnits

  frontend.io.contextLoadReq <> contextmem.io.contextLoadReq
  frontend.io.contextSaveReq <> contextmem.io.contextSaveReq
  contextmem.io.contextLoadRsp <> frontend.io.contextLoadRsp
  contextmem.io.contextSaveRsp <> frontend.io.contextSaveRsp

  contextmem.io.contextBase := io.csc.outVec
  contextmem.io.mainMem.memWrReq <> io.memPort(0).memWrReq
  contextmem.io.mainMem.memWrDat <> io.memPort(0).memWrDat
  io.memPort(0).memWrRsp <> contextmem.io.mainMem.memWrRsp
}
