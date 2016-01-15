package Seyrek

import Chisel._
import TidbitsStreams._

// base abstract class for semiring reduction op schedulers
// the scheduler is needed to avoid RAW hazards due to latency in
// loading, applying the add operation on and storing the context.
// additionally, the scheduler may issue dispatches out-of-order
// to increase utilization

// the scheduler maintains a window of size <SeyrekParams.issueWindow>
// to keep track of which "threads" (row indices) are in flight, i.e.
// have already entered and started processing.

class SchedulerIO(p: SeyrekParams) extends Bundle with SeyrekCtrlStat {
  // instructions are the value-input pair inputs to the scheduler
  // (input from the multiplier)
  val instr = Decoupled(p.vi).flip
  // dispatched instructions are ready to enter the reducer
  // (output to the reducer)
  val issue = Decoupled(p.vi)
  // completed instructions
  // (input from the reducer)
  val compl = Decoupled(p.i).flip
  // statistics
  val hazardStallCycles = UInt(OUTPUT, 32)
}

abstract class Scheduler(p: SeyrekParams) extends Module {
  val io = new SchedulerIO(p)

  // indicate how the scheduler behaves with its instructions scheduling
  def dispatchOrdered: Boolean  // whether dispatches are in-order
  def completeOrdered: Boolean  // whether completes are in-order

  // useful printfs to debug scheduler entry/exit, uncomment as needed
  /*
  when(io.instr.valid & io.instr.ready) {
    printf("enter scheduler: %d\n", io.instr.bits.ind)
  }


  when(io.compl.valid & io.compl.ready) {
    printf("exit scheduler: %d\n", io.compl.bits)
  }

  when(io.instr.valid & !io.instr.ready) {
    printf("can't enter scheduler: %d \n", io.instr.bits.ind)
  }

  when(io.compl.valid & !io.compl.ready) {
    printf("can't exit scheduler: %d\n", io.compl.bits)
  }
  */
}

// null scheduler: basically does nothing, just pushing the incoming
// instructions out directly as the dispatch stream. not really useful
// unless instructions are guaranteed to be RAW-hazard free.
class NullScheduler(p: SeyrekParams) extends Scheduler(p) {
  val dispatchOrdered = true
  val completeOrdered = true
  // dispatch incoming instructions directly
  io.instr <> io.issue
  // always ready to accept completed instructions
  io.compl.ready := Bool(true)
}
