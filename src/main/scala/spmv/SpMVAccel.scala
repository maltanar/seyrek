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

  backend.io.start := io.start
  frontend.io.start := io.start

  backend.io.mode := io.mode
  frontend.io.mode := io.mode

  io.finished := Mux(io.mode === SeyrekModes.START_REGULAR,
                    frontend.io.finished, backend.io.finished)

  // TODO expose more detailed status and statistics

  // TODO more flexible config for connecting mem ports
  // TODO interleave contextmem-backend reqs
  io.csc <> backend.io.csc
  backend.io.reqSeq <> io.memPort(0)

  io.csc <> frontend.io.csc
  backend.io.workUnits <> frontend.io.workUnits

  frontend.io.contextLoadReq <> backend.io.contextLoadReq
  frontend.io.contextSaveReq <> backend.io.contextSaveReq
  backend.io.contextLoadRsp <> frontend.io.contextLoadRsp
  backend.io.contextSaveRsp <> frontend.io.contextSaveRsp
}
