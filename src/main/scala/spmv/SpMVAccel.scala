package Seyrek

import Chisel._
import TidbitsPlatformWrapper._
import TidbitsStreams._

class SpMVProcElemIF(pSeyrek: SeyrekParams) extends Bundle {
  val start = Bool(INPUT)
  val mode = UInt(INPUT, width = 10)
  val finished = Bool(OUTPUT)
  val csc = new CSCSpMV(pSeyrek).asInput
  val contextReqCnt = UInt(INPUT, width = 10)
  // performance counter access
  val perfCtrSel = UInt(INPUT, width = 10)
  val perfCtrVal = UInt(OUTPUT, width = 32)
}

class SpMVAccel(p: PlatformWrapperParams, pSeyrek: SeyrekParams)
extends GenericAccelerator(p) {
  val numMemPorts = pSeyrek.numPEs * pSeyrek.portsPerPE
  val io = new GenericAcceleratorIF(numMemPorts, p) {
    val pe = Vec.fill(pSeyrek.numPEs) {new SpMVProcElemIF(pSeyrek)}
  }
  setName(pSeyrek.accelName)
  io.signature := makeDefaultSignature()

  var fullPerfCtrMap = scala.collection.mutable.Map[String, Int]()

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

    when(!ioPE.start) {regCycleCount := UInt(0)}
    .elsewhen(ioPE.start & !ioPE.finished) {
      regCycleCount := regCycleCount + UInt(1)
    }

    val yes = ioPE.start & !ioPE.finished
    // StreamMonitors for general progress monitoring
    // also output as printfs on the Chisel C++ emulator
    // TODO control the printfs
    val monWU = StreamMonitor(frontend.io.workUnits, yes, s"$i workUnits")
    val monCLQ = StreamMonitor(frontend.io.contextLoadReq, yes, s"$i contextLoadReq")
    val monCSQ = StreamMonitor(frontend.io.contextSaveReq, yes, s"$i contextSaveReq")
    val monCLP = StreamMonitor(frontend.io.contextLoadRsp, yes, s"$i contextLoadRsp")
    val monCSP = StreamMonitor(frontend.io.contextSaveRsp, yes, s"$i contextSaveRsp")

    // performance counter stuff
    val regPerfCtrSel = Reg(next = ioPE.perfCtrSel)

    val perfCtrMap = Map[String, Data](
      "cycleCount" -> regCycleCount,
      "hazardStallCycles" -> frontend.io.hazardStallCycles,
      "workUnits" -> monWU,
      "contextLoadReq" -> monCLQ,
      "contextStoreReq" -> monCSQ,
      "contextLoadRsp" -> monCLP,
      "contextStoreRsp" -> monCSP
    )

    var allocInd: Int = 0
    var ctrRegs = Seq[Data]()

    for((name, bits) <- perfCtrMap) {
      val flat = bits.flatten
      for((nameF, flatF) <- flat) {
        val fullName = name + "_" + nameF
        if(flatF.getWidth() > 32)
          throw new Exception("Performance counter can be max 32 bits")
        // TODO guaranteed that map traversal order and names will be same?
        fullPerfCtrMap(fullName) = allocInd
        allocInd += 1
        ctrRegs ++= Seq(Reg(next = flatF))
      }
    }

    ioPE.perfCtrVal := Vec(ctrRegs)(regPerfCtrSel)

    /*
    for(mp <- 0 until pSeyrek.portsPerPE) {
      StreamMonitor(backend.io.mainMem(mp).memRdReq, yes, s"$i memRdReq $mp")
      StreamMonitor(backend.io.mainMem(mp).memRdRsp, yes, s"$i memRdRsp $mp")
      StreamMonitor(backend.io.mainMem(mp).memWrReq, yes, s"$i memWrReq $mp")
      StreamMonitor(backend.io.mainMem(mp).memWrRsp, yes, s"$i memWrRsp $mp")
    }
    */
  }

  // generate a C++ function that returns a mapping from performance counter
  // names to performance counter registers
  def generatePerfCtrMapCode(targetDir: String) = {
    var driverStr: String = ""

    driverStr += "  map <string, unsigned int> getPerfCtrMap() {" + "\n"
    driverStr += "    map <string, unsigned int> perfCtrMap;" + "\n"
    for((name, ind) <- fullPerfCtrMap) {
      driverStr += "    perfCtrMap[\""+name+"\"] = "+s"$ind;" + "\n"
    }
    driverStr += "    return perfCtrMap;" + "\n"
    driverStr += "  }\n\n"

    import java.io._
    val writer = new PrintWriter(new File(targetDir+"/perfctr.hpp" ))
    writer.write(driverStr)
    writer.close()
    println("=======> Performance counter map written to perfctr.hpp")
  }
}
