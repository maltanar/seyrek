package Seyrek

import Chisel._
import TidbitsMath._

// combinatorial variants of UInt add and multiply
class OpAddCombinatorial(w: Int) extends BinaryMathOp(w) {
  io.out.bits := io.in.bits.first + io.in.bits.second
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  val latency = 0
}

class OpMulCombinatorial(w: Int) extends BinaryMathOp(w) {
  io.out.bits := io.in.bits.first * io.in.bits.second
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  val latency = 0
}

// generate operator (defined by fxn) with n-cycle latency
// mostly intended to simulate high-latency ops, won't make much sense
// in synthesis since all "useful work" is carried out before entering
// the delay pipe anyway
class StagedUIntOp(w: Int, n: Int, fxn: (UInt, UInt) => UInt)
extends BinaryMathOp(w) {
  val latency = n
  if(latency == 0) {
    println("StagedUIntOp needs at least 1 stage")
    System.exit(-1)
  }
  // connect transformed input to first stage
  val delayPipe = Vec.fill(n) {SystolicReg(w)}
  delayPipe(0).in.valid := io.in.valid
  delayPipe(0).in.bits := fxn(io.in.bits.first, io.in.bits.second)
  io.in.ready := delayPipe(0).in.ready
  // connect stages
  for(i <- 0 until n-1) {
    delayPipe(i+1).in <> delayPipe(i).out
  }
  // connect last stage to output
  io.out <> delayPipe(n-1).out
}

// TODO not a great way of doing conditional Verilog gen, find a better way
object isVerilog {
  def apply(): Boolean = {
    if(Driver.backend != null) {
      return (Driver.backend.getClass().getSimpleName() == "VerilogBackend")
    }
    else {return false}
  }
}

class DPAdder(stages: Int) extends BinaryMathOp(64) {
  val latency: Int = stages
  val enableBlackBox = isVerilog()

  if(enableBlackBox) {
    // generate blackbox for dbl-precision floating pt add
    val op = Module(new DPBlackBox("AddDbl"+stages.toString+"Stg")).io
    op.inA.bits := io.in.bits.first
    op.inB.bits := io.in.bits.second
    op.inA.valid := io.in.valid
    op.inB.valid := io.in.valid
    io.in.ready := op.inA.ready & op.inB.ready
    op.out <> io.out

  } else {
    val fxn : (UInt,UInt)=>UInt = {
      // interpret input as doubles, add, then interpret as UInt
      (a,b) => chiselCast(
                          chiselCast(a)(Dbl()) + chiselCast(b)(Dbl())
                         )(UInt())
    }
    val op = Module(new StagedUIntOp(64, stages, fxn)).io
    io <> op
  }
}


class DPMultiplier(stages: Int) extends BinaryMathOp(64) {
  val latency: Int = stages
  val enableBlackBox = isVerilog()

  if(enableBlackBox) {
    // generate blackbox for dbl-precision floating pt mul
    val op = Module(new DPBlackBox("MulDbl"+stages.toString+"Stg")).io
    op.inA.bits := io.in.bits.first
    op.inB.bits := io.in.bits.second
    op.inA.valid := io.in.valid
    op.inB.valid := io.in.valid
    io.in.ready := op.inA.ready & op.inB.ready
    op.out <> io.out
  } else {
    val fxn : (UInt,UInt)=>UInt = {
      // interpret input as doubles, mul, then interpret as UInt
      (a,b) => chiselCast(
                          chiselCast(a)(Dbl()) * chiselCast(b)(Dbl())
                         )(UInt())
    }
    val op = Module(new StagedUIntOp(64, stages, fxn)).io
    io <> op
  }
}

class DPBlackBox(name: String) extends BlackBox {
  moduleName = name
  val io = new Bundle {
    val inA = Decoupled(UInt(width = 64)).flip
    val inB = Decoupled(UInt(width = 64)).flip
    val out = Decoupled(UInt(width = 64))
  }

  def renameAsAXIStream(strm: DecoupledIO[UInt], nm: String, isMaster: Boolean) = {
    val pf = if(isMaster) "m_" else "s_"
    strm.valid.setName(pf + "axis_" + nm + "_tvalid")
    strm.ready.setName(pf + "axis_" + nm + "_tready")
    strm.bits.setName(pf + "axis_" + nm + "_tdata")
  }

  addClock(Driver.implicitClock)
  renameClock("clk", "aclk")

  renameAsAXIStream(io.inA, "a", false)
  renameAsAXIStream(io.inB, "b", false)
  renameAsAXIStream(io.out, "result", true)
}
