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

  // (v, v, i) -> [mul] -> (n = v*v, i)
  io.workUnits <> mul.in

  // (n, i) -> [queue] -> [scheduler]
  Queue(mul.out, 2) <> sched.io.instr

  // [scheduler] -> (n, i) -> [load context]
  sched.io.issue <> io.contextLoadReq

  // [load context] -> (o, n, i) -> [add]
  io.contextLoadRsp <> add.in

  // [add] -> (s = o+n, i) -> [queue] -> [save context]
  Queue(add.out, 2) <> io.contextSaveReq

  // signal completion to remove from scheduler
  // [save context] -> i -> [scheduler]
  io.contextSaveRsp <> sched.io.compl

  // TODO wire up start/finished logic
  // TODO add statistics
}
