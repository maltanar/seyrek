package Seyrek

import Chisel._
import TidbitsTestbenches._
import TidbitsOCM._
import TidbitsStreams._
import TidbitsSimUtils._
import TidbitsAXI._
import TidbitsDMA._
import TidbitsPlatformWrapper._
import TidbitsMath._

object ChannelConfigs {
  val csrTest = Map(
    "ptrs" -> ReadChanParams(maxReadTxns = 8, port = 0),
    "inds" -> ReadChanParams(maxReadTxns = 16, port = 1),
    "nzdata" -> ReadChanParams(maxReadTxns = 16, port = 2),
    "inpvec" -> ReadChanParams(maxReadTxns = 8, port = 3)
  )
}

class CSRTestAParams(p: PlatformWrapperParams) extends SeyrekParams {
  val accelName = "CSRTestA"
  val numPEs = 8
  val portsPerPE = 4
  val chanConfig = ChannelConfigs.csrTest
  val indWidth = 32
  val valWidth = 64
  val mrp = p.toMemReqParams()

  val makeSemiringAdd = { () =>
    new StagedUIntOp(valWidth, 1, {(a: UInt, b: UInt) => a+b})
  }

  val makeSemiringMul = { () =>
    new SystolicSInt64Mul_5Stage()
  }
  val issueWindow = 16
}

class CSRTestBParams(p: PlatformWrapperParams) extends SeyrekParams {
  val accelName = "CSRTestB"
  val numPEs = 4
  val portsPerPE = 4
  val chanConfig = ChannelConfigs.csrTest
  val indWidth = 32
  val valWidth = 64
  val mrp = p.toMemReqParams()

  val makeSemiringAdd = { () =>
    new StagedUIntOp(valWidth, 1, {(a: UInt, b: UInt) => a+b})
  }

  val makeSemiringMul = { () =>
    new SystolicSInt64Mul_5Stage()
  }
  val issueWindow = 16
}

object SeyrekMainObj {
  type AccelInstFxn = PlatformWrapperParams => SpMVAccelCSR
  type AccelMap = Map[String, AccelInstFxn]
  type PlatformInstFxn = AccelInstFxn => PlatformWrapper
  type PlatformMap = Map[String, PlatformInstFxn]

  val accelMap: AccelMap  = Map(
    "CSRTestA" -> {p => new SpMVAccelCSR(p, new CSRTestAParams(p))},
    "CSRTestB" -> {p => new SpMVAccelCSR(p, new CSRTestBParams(p))}
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
    val regDrvRoot = "src/main/scala/fpga-tidbits/platform-wrapper/regdriver/"
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
    val regDrvRoot = "src/main/scala/fpga-tidbits/platform-wrapper/regdriver/"
    val files = Array("wrapperregdriver.h", "platform-tester.cpp",
      "platform.h", "testerdriver.hpp")
    for(f <- files) { fileCopy(regDrvRoot + f, "emulator/" + f) }

    copySeyrekFiles("Tester", "emulator/")
  }

  def showHelp() = {
    println("Usage: run <op> <accel> <platform>")
    println("where:")
    println("<op> = verilog software emulator")
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
    } else {
      showHelp()
      return
    }
  }
}
