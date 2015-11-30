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

class SystolicRegParams[TI <: Data, TO <: Data](
  val tIn: TI, // wIn: width of input stream in bits
  val tOut: TO, // wOut: width of output stream in bits
  val fxn: TI => TO // fxn: function to apply on the way out
)

class SystolicReg[TI <: Data, TO <: Data](val p: SystolicRegParams[TI, TO])
extends Module {
  val io = new Bundle {
    val in = Decoupled(p.tIn.cloneType).flip
    val out = Decoupled(p.tOut.cloneType)
  }
  val regValid = Reg(init = Bool(false))
  val resetVal = UInt(0, width = p.tOut.getWidth())
  val regData = RegInit[TO](p.tOut.fromBits(resetVal))
  val allowNewData = (!regValid || io.out.ready)

  io.out.bits := regData
  io.out.valid := regValid
  io.in.ready := allowNewData
  // somehow this needs to be outside the when (mux) below,
  // otherwise Chisel complains about "no default value on wire"
  val updData: TO = p.fxn(io.in.bits)

  when(allowNewData) {
    regData := updData
    regValid := io.in.valid
  }
}

// convenience constructor for SystolicReg
object SystolicReg {
  def apply(w: Int) = {
    val uintP = new SystolicRegParams[UInt, UInt](
      UInt(width = w), UInt(width = w), fxn = {x: UInt => x}
    )
    Module(new SystolicReg[UInt, UInt](uintP)).io
  }
  def apply[TI <: Data, TO <: Data](tIn: TI, tOut: TO, fxn: TI => TO) = {
    val p = new SystolicRegParams[TI,TO](tIn, tOut, fxn)
    Module(new SystolicReg[TI,TO](p)).io
  }

  def apply[TI <: Data, TO <: Data](tIn: TI, tOut: TO, fxn: TI => TO,
    in: DecoupledIO[TI]
  ) = {
    val p = new SystolicRegParams[TI,TO](tIn, tOut, fxn)
    val mod = Module(new SystolicReg[TI,TO](p)).io
    in <> mod.in
    mod.out
  }
}

class PipelinedMultStageData(w: Int, wMul: Int) extends Bundle {
  val signA = Bool()
  val a = UInt(width = w)
  val signB = Bool()
  val b = UInt(width = w)
  val mulRes = UInt(width = 2*wMul)
  val addRes = UInt(width = w)

  override def cloneType: this.type = new PipelinedMultStageData(w, wMul).asInstanceOf[this.type]
}

class SystolicTest extends SemiringOp(64) {
  val latency = 16
  val wMul: Int = 32
  val metad = new PipelinedMultStageData(64, wMul)

  // stage 0: convert to signed magnitude form
  val fxnS0 = {i: SemiringOperands => val m = new PipelinedMultStageData(64, wMul)
    m.signA := i.first(63)
    m.signB := i.second(63)
    m.a := Mux(i.first(63), UInt(~i.first + UInt(1), width = 64), i.first)
    m.b := Mux(i.second(63), UInt(~i.second + UInt(1), width = 64), i.second)
    m.mulRes := UInt(0)
    m.addRes := UInt(0)
    m
  }
  val s0 = SystolicReg(io.in.bits, metad, fxnS0, io.in)

  val fMaker = { (offA: Int, offB: Int) =>
    {i: PipelinedMultStageData => val m = new PipelinedMultStageData(64, wMul)
      m := i
      m.mulRes := i.a((wMul*(offA+1))-1, wMul*offA) * i.b((wMul*(offB+1))-1, wMul*offB)
      m.addRes := i.mulRes + i.addRes
      m
    }
  }

  val s1 = SystolicReg(metad, metad, fMaker(0, 0), s0)
  val s2 = SystolicReg(metad, metad, fMaker(0, 1), s1)
  val s3 = SystolicReg(metad, metad, fMaker(1, 0), s2)

  // convert back to 2s complement on the way out
  s3.ready := io.out.ready
  io.out.valid := s3.valid

  val magnRes = Cat(UInt(0, width=1), s3.bits.addRes(62, 0))
  val isResultNegative = s3.bits.signA ^ s3.bits.signB
  io.out.bits := Mux(isResultNegative, ~magnRes + UInt(1), magnRes)
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
