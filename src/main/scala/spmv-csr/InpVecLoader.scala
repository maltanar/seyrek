package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._

// TODO there are various similarities between this and ContextMem for col-major
// can we come up with an abstraction to unify the designs?

class InpVecLoaderIO(p: SeyrekParams) extends Bundle with SeyrekCtrlStat {
  // how many outstanding mem reqs should be allocated (if applicable)
  val contextReqCnt = UInt(INPUT, 10)
  // pointer to start of x
  val contextBase = UInt(INPUT, width = p.mrp.addrWidth)
  // x load requests -- (v, i, j, rl)
  val loadReq = Decoupled(p.viil).flip
  // x load responses -- (v, x[j], i, rl)
  val loadRsp = Decoupled(p.wul)
  // main memory access port
  val mainMem = new GenericMemoryMasterPort(p.mrp)
  // stat ports
  val cacheNewReq = new StreamMonitorOutIF()
}

// base abstract class for input vec. loaders
abstract class InpVecLoader(val p: SeyrekParams) extends Module {
  val io = new InpVecLoaderIO(p)

  // whether the InpVecLoader responds to load requests in-order
  def inOrder: Boolean
}
