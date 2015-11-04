package Seyrek

import Chisel._
import TidbitsStreams._

class SpMVFrontendIO(p: SeyrekParams) extends Bundle with SeyrekCtrlStat {
  val csc = new CSCSpMV(p).asInput
  // input from backend
  val workUnits = Decoupled(p.wu).flip
  // context access ports
  val contextLoadReq = Decoupled(p.i)
  val contextLoadRsp = Decoupled(p.vi).flip
  val contextSaveReq = Decoupled(p.vi)
  val contextSaveRsp = Decoupled(p.i).flip
}

class SpMVFrontend(p: SeyrekParams) extends Module {
  val io = new SpMVFrontendIO(p)

  val add = Module(p.makeSemiringAdd())
  val mul = Module(p.makeSemiringMul())

  // TODO add schedular
  // TODO add queues and connect to context load/save ports
  // TODO wire up start/finished logic
}
