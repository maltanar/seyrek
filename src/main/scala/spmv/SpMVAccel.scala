package Seyrek

import Chisel._
import TidbitsPlatformWrapper._
import TidbitsStreams._

class SpMVProcElemIF(pSeyrek: SeyrekParams) extends Bundle {
  val start = Bool(INPUT)
  val mode = UInt(INPUT, width = 10)
  val finished = Bool(OUTPUT)
  val csc = new CSCSpMV(pSeyrek).asInput
  val cycleCount = UInt(OUTPUT, width = 32)
  val contextReqCnt = UInt(INPUT, width = 10)
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

    backend.io.contextReqCnt := ioPE.contextReqCnt
    backend.io.start := ioPE.start
    frontend.io.start := ioPE.start

    backend.io.mode := ioPE.mode
    frontend.io.mode := ioPE.mode

    ioPE.finished := Mux(ioPE.mode === SeyrekModes.START_REGULAR,
                      frontend.io.finished, backend.io.finished)

    ioPE.csc <> backend.io.csc
    for(mp <- 0 until pSeyrek.portsPerPE)
      backend.io.mainMem(mp) <> io.memPort(i * pSeyrek.portsPerPE + mp)

    ioPE.csc <> frontend.io.csc
    backend.io.workUnits <> frontend.io.workUnits

    frontend.io.contextLoadReq <> backend.io.contextLoadReq
    frontend.io.contextSaveReq <> backend.io.contextSaveReq
    backend.io.contextLoadRsp <> frontend.io.contextLoadRsp
    backend.io.contextSaveRsp <> frontend.io.contextSaveRsp

    // keep a per-PE cycle count register, tracks time start -> finished
    val regCycleCount = Reg(init = UInt(0, 32))
    ioPE.cycleCount := regCycleCount
    when(!ioPE.start) {regCycleCount := UInt(0)}
    .elsewhen(ioPE.start & !ioPE.finished) {
      regCycleCount := regCycleCount + UInt(1)
    }

    val yes = Bool(true)
    // StreamMonitors for general progress monitoring -- uncomment to enable
    // and read output as printfs on the Chisel C++ emulator
    /*
    StreamMonitor(frontend.io.workUnits, yes, s"$i workUnits")
    StreamMonitor(frontend.io.contextLoadReq, yes, s"$i contextLoadReq")
    StreamMonitor(frontend.io.contextSaveReq, yes, s"$i contextSaveReq")
    StreamMonitor(frontend.io.contextLoadRsp, yes, s"$i contextLoadRsp")
    StreamMonitor(frontend.io.contextSaveRsp, yes, s"$i contextSaveRsp")
    */
    /*
    for(mp <- 0 until pSeyrek.portsPerPE) {
      StreamMonitor(backend.io.mainMem(mp).memRdReq, yes, s"$i memRdReq $mp")
      StreamMonitor(backend.io.mainMem(mp).memRdRsp, yes, s"$i memRdRsp $mp")
      StreamMonitor(backend.io.mainMem(mp).memWrReq, yes, s"$i memWrReq $mp")
      StreamMonitor(backend.io.mainMem(mp).memWrRsp, yes, s"$i memWrRsp $mp")
    }
    */

  }

  // TODO expose more detailed status and statistics
}
