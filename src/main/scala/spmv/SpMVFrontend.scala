package Seyrek

import Chisel._
import TidbitsStreams._

class SpMVFrontendIO(p: SeyrekParams) extends Bundle with SeyrekCtrlStat {
  val csc = new CSCSpMV(p).asInput
  // input from backend
  val workUnits = Decoupled(p.wu).flip
  // context access ports
  val contextLoadReq = Decoupled(p.vi)
  val contextLoadRsp = Decoupled(p.wu).flip
  val contextSaveReq = Decoupled(p.vi)
  val contextSaveRsp = Decoupled(p.i).flip
}

class SpMVFrontend(p: SeyrekParams) extends Module {
  val io = new SpMVFrontendIO(p)

  val mul = Module(new ContextfulSemiringOp(p, p.makeSemiringMul)).io
  val add = Module(new ContextfulSemiringOp(p, p.makeSemiringAdd)).io
  val sched = Module(p.makeScheduler())

  // TODO do we really need queues at every step, and how big?

  // (v, v, i) -> [queue] -> [mul] -> (n = v*v, i)
  Queue(io.workUnits, 2) <> mul.in

  // (n, i) -> [queue] -> [scheduler]
  Queue(mul.out, 2) <> sched.io.instr

  // [scheduler] -> (n, i) -> [load context]
  Queue(sched.io.issue, 2) <> io.contextLoadReq

  // [load context] -> (o, n, i) -> [add]
  Queue(io.contextLoadRsp, 2) <> add.in

  // [add] -> (s = o+n, i) -> [queue] -> [save context]
  Queue(add.out, 2) <> io.contextSaveReq

  // signal completion to remove from scheduler
  // [save context] -> i -> [scheduler]
  Queue(io.contextSaveRsp, 2) <> sched.io.compl

  // completion logic
  io.finished := Bool(false)
  val regCompletedOps = Reg(init = UInt(0, 32))

  val sIdle :: sRunning :: sFinished :: Nil = Enum(UInt(), 3)
  val regState = Reg(init = UInt(sIdle))

  switch(regState) {
      is(sIdle) {
        when((io.mode === SeyrekModes.START_REGULAR) & io.start) {
          regState := sRunning
          regCompletedOps := UInt(0)
        }
      }

      is(sRunning) {
        // count completed operations
        when (sched.io.compl.ready & sched.io.compl.valid) {
          regCompletedOps := regCompletedOps + UInt(1)
        }
        when (regCompletedOps === io.csc.nz) { regState := sFinished }
      }

      is(sFinished) {
        io.finished := Bool(true)
        when (!io.start) {regState := sIdle}
      }
  }

  // TODO add statistics
}
