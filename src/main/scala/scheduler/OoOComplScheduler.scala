package Seyrek

import Chisel._
import TidbitsStreams._

// scheduler with in-order dispatch, out-of-order completion

class OoOComplScheduler(p: SeyrekParams) extends Scheduler(p) {
  val dispatchOrdered = true
  val completeOrdered = false

  // use a StreamCAM to keep track of in-flight instructions
  // non-unique elements won't be allowed to enter, but once entered
  // elements can be removed in any order (hence CAM)
  val inFlight = Module(new StreamCAM(p.issueWindow, p.indWidth)).io

  // the forker synchronizes the issue window queue entry and instruction
  // dispatch (both inFlight and dispatch must say ready before dispatch
  // actually happens)
  val forker = Module(new StreamFork(
    genIn = p.vi, genA = p.vi, genB = UInt(width = p.indWidth),
    forkA = {x: ValIndPair => x},
    forkB = {x: ValIndPair => x.ind}
  )).io

  io.instr <> forker.in
  forker.outA <> io.issue
  forker.outB <> inFlight.in

  io.compl <> inFlight.rm

  // hazard counting logic
  val regHazardStalls = Reg(init = UInt(0, 32))
  val regStart = Reg(next = io.start)
  when(!regStart & io.start) { regHazardStalls := UInt(0)}
  .elsewhen(regStart & inFlight.hazard) {
    regHazardStalls := regHazardStalls + UInt(1)
  }
  io.hazardStallCycles := regHazardStalls
}
