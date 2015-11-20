package Seyrek

import Chisel._
import TidbitsPlatformWrapper._

class SpMVProcElemIF(pSeyrek: SeyrekParams) extends Bundle {
  val start = Bool(INPUT)
  val mode = UInt(INPUT, width = 10)
  val finished = Bool(OUTPUT)
  val csc = new CSCSpMV(pSeyrek).asInput
}

class SpMVAccel(p: PlatformWrapperParams, pSeyrek: SeyrekParams)
extends GenericAccelerator(p) {
  val numMemPorts = pSeyrek.numPEs * pSeyrek.portsPerPE
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val pe = Vec.fill(pSeyrek.numPEs) {new SpMVProcElemIF(pSeyrek)}
  }
  setName(pSeyrek.accelName)
  io.signature := makeDefaultSignature()

  for(i <- 0 until pSeyrek.numPEs) {
    val backend = Module(new SpMVBackend(pSeyrek))
    val frontend = Module(new SpMVFrontend(pSeyrek))
    val ioPE = io.pe(i)

    backend.io.start := ioPE.start
    frontend.io.start := ioPE.start

    backend.io.mode := ioPE.mode
    frontend.io.mode := ioPE.mode

    ioPE.finished := Mux(ioPE.mode === SeyrekModes.START_REGULAR,
                      frontend.io.finished, backend.io.finished)

    ioPE.csc <> backend.io.csc
    backend.io.reqSeq <> io.memPort(i)
    // TODO more flexible config for connecting mem ports

    ioPE.csc <> frontend.io.csc
    backend.io.workUnits <> frontend.io.workUnits

    frontend.io.contextLoadReq <> backend.io.contextLoadReq
    frontend.io.contextSaveReq <> backend.io.contextSaveReq
    backend.io.contextLoadRsp <> frontend.io.contextLoadRsp
    backend.io.contextSaveRsp <> frontend.io.contextSaveRsp
  }

  // TODO expose more detailed status and statistics
}
