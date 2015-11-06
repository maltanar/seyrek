package Seyrek

import Chisel._
import TidbitsStreams._

// TODO expose hazard statistics?

// scheduler with in-order dispatch and completion

class InOrderScheduler(p: SeyrekParams) extends Scheduler(p) {
  val dispatchOrdered = true
  val completeOrdered = true

  val hqType = new OperandWithID(p.valWidth, p.indWidth)
  // use a UniqueQueue to keep track of in-flight instructions
  // this queue simply doesn't allow non-unique elements to enter
  val inFlight = Module(new UniqueQueue(p.valWidth, p.indWidth, p.issueWindow)).io
  // the forker synchronizes the issue window queue entry and instruction
  // dispatch (both queue and dispatch must say ready before dispatch
  // actually happens)
  val forker = Module(new StreamFork(
    genIn = p.vi, genA = p.vi, genB = hqType,
    forkA = {x: ValIndPair => x},
    forkB = {x: ValIndPair => OperandWithID(x.value, x.ind)}
  )).io

  io.instr <> forker.in
  forker.outA <> io.issue
  forker.outB <> inFlight.enq

  // completions cause elements to be dequeued from the in-flight window
  // since we assume in-order completion, we don't do any value checking
  inFlight.deq.ready := io.compl.valid
  io.compl.ready := inFlight.deq.valid
}
