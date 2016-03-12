package Seyrek

import Chisel._
import fpgatidbits.ocm._
import fpgatidbits.streams._
import fpgatidbits.dma._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.math._
import fpgatidbits.TidbitsMakeUtils

object ChannelConfigs {
  val csrTest = Map(
    "ptrs" -> ReadChanParams(maxReadTxns = 8, port = 2),
    "inds" -> ReadChanParams(maxReadTxns = 8, port = 3),
    "nzdata" -> ReadChanParams(maxReadTxns = 8, port = 0),
    "inpvec" -> ReadChanParams(maxReadTxns = 32, port = 1)
  )
}

class CSRTestParams(p: PlatformWrapperParams) extends SeyrekParams {
  val accelName = "CSRTest"
  val numPEs = 1
  val portsPerPE = 4
  val chanConfig = ChannelConfigs.csrTest
  val indWidth = 32
  val valWidth = 64
  val mrp = p.toMemReqParams()

  val makeSemiringAdd = { () => new DPAdder(6) }
  val makeSemiringMul = { () => new DPMultiplier(8) }

  val issueWindow = 8
}

object SeyrekMainObj {
  val tidbitsRoot: String = "fpga-tidbits/src/main/"
  type AccelInstFxn = PlatformWrapperParams => SpMVAccelCSR
  type AccelMap = Map[String, AccelInstFxn]
  type PlatformInstFxn = AccelInstFxn => PlatformWrapper
  type PlatformMap = Map[String, PlatformInstFxn]

  val accelConfigMap: AccelMap  = Map(
    "CSRTest" -> {p => new SpMVAccelCSR(p, new CSRTestParams(p))}
  )

  val platformMap: PlatformMap = Map(
    "ZedBoard" -> {f => new ZedBoardWrapper(f)},
    "ZedBoardLinux" -> {f => new ZedBoardLinuxWrapper(f)},
    "WX690T" -> {f => new WolverinePlatformWrapper(f)},
    "Tester" -> {f => new TesterWrapper(f)}
  )

  def fileCopy(from: String, to: String) = {
    import java.io.{File,FileInputStream,FileOutputStream}
    val src = new File(from)
    val dest = new File(to)
    new FileOutputStream(dest) getChannel() transferFrom(
      new FileInputStream(src) getChannel, 0, Long.MaxValue )
  }

  def makeVerilog(args: Array[String]) = {
    val accelName = args(0)
    val platformName = args(1)
    val accInst = accelConfigMap(accelName)
    val platformInst = platformMap(platformName)
    val chiselArgs = Array("--backend", "v")

    chiselMain(chiselArgs, () => Module(platformInst(accInst)))
  }

  def copySeyrekFiles(platformName: String, destDir: String) = {
    // copy Seyrek support files
    val seyrekDrvRoot = "src/main/cpp/"
    val seyrekFiles = Array("commonsemirings.hpp", "HWCSRSpMV.hpp",
      "semiring.hpp", "wrapperregdriver.h", "CSR.hpp", "main-csr.cpp",
      "CSRSpMV.hpp", "platform.h", "SWCSRSpMV.hpp",
      "seyrekconsts.hpp", "ParallelHWCSRSpMV.hpp")
    for(f <- seyrekFiles) { fileCopy(seyrekDrvRoot + f, destDir + f) }
    val seyrekSpMap = Map(
      "ZedBoardLinux" -> "seyrek-zed-linux.cpp",
      "Tester" -> "seyrek-tester.cpp"
    )
    val seyrekSpFile = seyrekSpMap(platformName)
    fileCopy(seyrekDrvRoot+seyrekSpFile, destDir + seyrekSpFile)
  }

  def makeSWPackage(args: Array[String]) = {
    val accelName = args(0)
    val platformName = args(1)
    val accInst = accelConfigMap(accelName)
    val platformInst = platformMap(platformName)
    val folderName = platformName + "-" + accelName + "/"
    // create target directory if missing
    import java.io.File
    val theDir = new File(folderName)
    if (!theDir.exists()) theDir.mkdir()

    // generate the platform reg wrapper driver -- not useful for now
    platformInst(accInst).generateRegDriver(folderName)
    // generate the performance counter mappings - platform doesn't matter here
    accInst(TesterWrapperParams).generatePerfCtrMapCode(folderName)
    // copy driver files for platform and Seyrek files
    val drvFiles: Array[String] = platformInst(accInst).platformDriverFiles
    val regDrvRoot = s"$tidbitsRoot/cpp/platform-wrapper-regdriver/"
    for(f <- drvFiles) {fileCopy(regDrvRoot+f, folderName+f)}
    copySeyrekFiles(platformName, folderName)
  }

  def makeEmulator(args: Array[String]) = {
    val accelName = args(0)

    val accInst = accelConfigMap(accelName)
    val platformInst = platformMap("Tester")
    val chiselArgs = Array("--backend","c","--targetDir", "emulator")

    chiselMain(chiselArgs, () => Module(platformInst(accInst)))
    // build driver
    platformInst(accInst).generateRegDriver("emulator/")
    accInst(TesterWrapperParams).generatePerfCtrMapCode("emulator/")
    // copy emulator driver and SW support files
    val regDrvRoot = s"$tidbitsRoot/cpp/platform-wrapper-regdriver/"
    val files = Array("wrapperregdriver.h", "platform-tester.cpp",
      "platform.h", "testerdriver.hpp")
    for(f <- files) { fileCopy(regDrvRoot + f, "emulator/" + f) }

    copySeyrekFiles("Tester", "emulator/")
  }

  def showHelp() = {
    println("Usage: run <op> <accel> <platform>")
    println("where:")
    println("<op> = (v)erilog (s)oftware (e)mulator ve(r)ilator")
    println("<accel> = " + accelConfigMap.keys.reduce({_ + " " +_}))
    println("<platform> = " + platformMap.keys.reduce({_ + " " +_}))
  }

  def main(args: Array[String]): Unit = {
    if (args.size != 3) {
      showHelp()
      return
    }

    val op = args(0)
    val rst = args.drop(1)

    if (op == "verilog" || op == "v") {
      makeVerilog(rst)
    } else if (op == "software" || op == "s") {
      makeSWPackage(rst)
    } else if (op == "emulator" || op == "e") {
      makeEmulator(rst)
    } else if (op == "verilator" || op == "r") {
      val accInst = accelConfigMap(rst(0))
      val platformInst = platformMap("Tester")
      TidbitsMakeUtils.makeVerilator(accInst, tidbitsRoot, "verilator")
      copySeyrekFiles("Tester", "verilator/")
      accInst(TesterWrapperParams).generatePerfCtrMapCode("verilator/")
    } else {
      showHelp()
      return
    }
  }
}
