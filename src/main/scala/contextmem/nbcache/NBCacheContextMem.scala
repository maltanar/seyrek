package Seyrek

import Chisel._
import TidbitsDMA._
import TidbitsStreams._
import TidbitsOCM._

class NBCacheContextMemParams(
  val numWordsInLine: Int,
  val readTxns: Int,
  val writeTxns: Int,
  idBits: Int,
  dataBits: Int,
  chanID: Int,
  mrp: MemReqParams
) extends ContextMemParams(idBits, dataBits, chanID, mrp)

class NBCacheContextMem(p: NBCacheContextMemParams) extends ContextMem(p) {
  val inOrder: Boolean = false

  // instantiate and connect load and store units
  val lu = Module(new LoadUnit(
    mrp = mrp, lineSizeBits = dataBits * numWordsInLine, chanIDBase = chanID,
    txns = readTxns
  )).io

  val su = Module(new StoreUnit(
    mrp = mrp, lineSizeBits = dataBits * numWordsInLine, chanIDBase = chanID,
    txns = writeTxns
  )).io
  // avoid RAW hazards on the main memory level
  su.checkAddr := lu.checkAddr
  lu.checkFound := su.checkFound
  // main memory access for load and store units
  lu.memRdReq <> io.mainMem.memRdReq
  io.mainMem.memRdRsp <> lu.memRdRsp
  su.memWrReq <> io.mainMem.memWrReq
  su.memWrDat <> io.mainMem.memWrDat
  io.mainMem.memWrRsp <> su.memWrRsp

  // TODO out-of-order cloakroom for storing the req contexts
  // TODO instantiate data and tag memories, lookup & result queue
  // TODO activate appropriate units as result of lookup
  // TODO logic for tracking pending misses
  // TODO logic for handling load responses and pending misses
  // TODO remove context from cloakroom and issue response

}
