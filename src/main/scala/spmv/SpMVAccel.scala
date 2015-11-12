package Seyrek

import Chisel._
import TidbitsPlatformWrapper._

class SpMVAccel(p: PlatformWrapperParams, pSeyrek: SeyrekParams)
extends GenericAccelerator(p) {
  val numMemPorts = 1 // TODO should be made configurable
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val start = Bool(INPUT)
    val mode = UInt(INPUT, width = 10)
    val finished = Bool(OUTPUT)
    val csc = new CSCSpMV(pSeyrek).asInput
  }
  setName(pSeyrek.accelName)
  io.signature := makeDefaultSignature()

  val backend = Module(new SpMVBackend(pSeyrek))
  val frontend = Module(new SpMVFrontend(pSeyrek))
  val contextmem = Module(pSeyrek.makeContextMemory())

  backend.io.start := io.start
  frontend.io.start := io.start
  contextmem.io.start := io.start

  backend.io.mode := io.mode
  frontend.io.mode := io.mode
  contextmem.io.mode := io.mode

  io.finished := Mux(io.mode === SeyrekModes.START_REGULAR,
                    frontend.io.finished, contextmem.io.finished)

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
