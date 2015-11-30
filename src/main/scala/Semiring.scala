package Seyrek

import Chisel._

// base definitions for operands and operators for semirings

class SemiringOperands(val w: Int) extends Bundle {
  val first = UInt(width = w)
  val second = UInt(width = w)

  override def clone = {
    new SemiringOperands(w).asInstanceOf[this.type]
  }
}

object SemiringOperands {
  def apply(first: UInt, second: UInt) = {
    if(first.getWidth() != second.getWidth()) {
      throw new Exception("Operand widths do not match")
    }
    val sop = new SemiringOperands(first.getWidth())
    sop.first := first
    sop.second := second
    sop
  }
}

class SemiringOpIO(w: Int) extends Bundle {
  val in = Decoupled(new SemiringOperands(w)).flip
  val out = Decoupled(UInt(width = w))
}

// base class for semiring operators
// exposes a Valid-wrapped (UInt, UInt) => UInt interface, and the op latency
abstract class SemiringOp(val w: Int) extends Module {
  val io = new SemiringOpIO(w)
  def latency: Int
}

// combinatorial variants of UInt add and multiply
class OpAddCombinatorial(w: Int) extends SemiringOp(w) {
  io.out.bits := io.in.bits.first + io.in.bits.second
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  val latency = 0
}

class OpMulCombinatorial(w: Int) extends SemiringOp(w) {
  io.out.bits := io.in.bits.first * io.in.bits.second
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  val latency = 0
}

// systolic reg to parametrize op stages flexibly
// this essentially behaves like a single-element queue with no
// fallthrough, so it can be used to add forced latency to an op
// the ready signal is still combinatorially linked to allow fast
// handshakes, like a Chisel queue with pipe=true flow=false
// supports defining a transfer function from input to output, to support
// building decoupled pipelined operators
// parameters:

class SystolicRegParams(
  val wIn: Int, // wIn: width of input stream in bits
  val wOut: Int, // wOut: width of output stream in bits
  val fxn: UInt => UInt = {x => x} // fxn: function to apply on the way out
)

class SystolicReg(val p: SystolicRegParams) extends Module {
  val io = new Bundle {
    val in = Decoupled(UInt(width = p.wIn)).flip
    val out = Decoupled(UInt(width = p.wOut))
  }
  val regValid = Reg(init = Bool(false))
  val regData = Reg(init = UInt(0, p.wOut))
  val allowNewData = (!regValid || io.out.ready)

  io.out.bits := regData
  io.out.valid := regValid
  io.in.ready := allowNewData

  when(allowNewData) {
    regData := p.fxn(io.in.bits)
    regValid := io.in.valid
  }
}

// convenience constructor for SystolicReg
object SystolicReg {
  def apply(w: Int) = {Module(new SystolicReg(new SystolicRegParams(w, w))).io}
}

// convenienve constructor for SystolicRegParams
object SystolicStage {
  def apply(wIn: Int, wOut: Int, fxn: UInt => UInt) = {
    new SystolicRegParams(wIn, wOut, fxn)
  }
}

// creates a sequence of systolic reg stages as described the <stages> param
class SystolicPipelinedOp(stages: Seq[SystolicRegParams]) extends Module {
  val numStages = stages.size
  val io = new Bundle {
    val in = Decoupled(UInt(width = stages(0).wIn)).flip
    val out = Decoupled(UInt(width = stages(numStages-1).wOut))
  }
  // instantiate all the stages with the appropriate params
  val stageRegs = Vec.tabulate(numStages) {
    i: Int => Module(new SystolicReg(stages(i))).io
  }
  // connect the first stage input to the main input
  io.in <> stageRegs(0).in
  // connect i/o of all intermediate stages
  for(i <- 0 until numStages-1) { stageRegs(i).out <> stageRegs(i+1).in }
  // connect main output to last stage output
  stageRegs(numStages-1).out <> io.out
}

class PipelinedInt64Mul extends SemiringOp(64) {
  val latency = 3
  // TODO add support for flow control -- systolic regs or handshaking
  // across latency
  // TODO better/more parametrized structure

  // convenience mux generator for turning two's complement numbers into
  // magnitude (return value) + sign (highest bit of input)
  def twosComplToMagnitude(in: UInt, w: Int): UInt = {
    Mux(in(w-1), UInt(~in + UInt(1), width = w), in)
  }
  // turn into sign+magnitude form and register inputs
  val regSignA = ShiftRegister(in = io.in.bits.first(63), n = 3)
  val regSignB = ShiftRegister(in = io.in.bits.second(63), n = 3)
  val regValA = Reg(next = twosComplToMagnitude(io.in.bits.first, 64))
  val regValB = Reg(next = twosComplToMagnitude(io.in.bits.second, 64))
  // useful to have the upper-lower parts of words
  val a0 = regValA(31, 0)
  val a1 = regValA(63, 32)
  val b0 = regValB(31, 0)
  val b1 = regValB(63, 32)
  // compute the partial products
  val regValA0B0 = Reg(init = UInt(0, width = 64), next = a0 * b0)
  // a0b1 and a1b0 are truncated to 32 bits, since these will be left-shifted
  // 32 bits before the reduction
  val regValA0B1 = Reg(init = UInt(0, width = 32), next = a0 * b1)
  val regValA1B0 = Reg(init = UInt(0, width = 32), next = a1 * b0)
  // don't calculate a1b1 since we don't keep track of overflow bits

  val resLower = regValA0B0(31, 0)
  val resUpper = regValA0B0(63, 32) + regValA0B1 + regValA1B0
  val magnRes = Cat(UInt(0, width=1), resUpper(30, 0), resLower)
  val regMagnRes = Reg(init = UInt(0, width = 64), next = magnRes)

  val isResultNegative = regSignA ^ regSignB
  val res = Mux(isResultNegative, ~regMagnRes + UInt(1), regMagnRes)

  io.out.bits := res
}

// generate operator (defined by fxn) with n-cycle latency
// mostly intended to simulate high-latency ops, won't make much sense
// in synthesis since all "useful work" is carried out before entering
// the delay pipe anyway
class StagedUIntOp(w: Int, n: Int, fxn: (UInt, UInt) => UInt)
extends SemiringOp(w) {
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

class DPAdder(stages: Int) extends SemiringOp(64) {
  val latency: Int = stages
  val enableBlackBox = isVerilog()

  if(enableBlackBox) {
    // generate blackbox for dbl-precision floating pt add
    val op = Module(new DPBlackBox("WrapperXilinxDPAdd"+stages.toString+"Stage")).io
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


class DPMultiplier(stages: Int) extends SemiringOp(64) {
  val latency: Int = stages
  val enableBlackBox = isVerilog()

  if(enableBlackBox) {
    // TODO generate blackbox for dbl-precision floating pt mul
    val op = Module(new DPBlackBox("WrapperXilinxDPMul"+stages.toString+"Stage")).io
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
  this.addClock(Driver.implicitClock)
}
