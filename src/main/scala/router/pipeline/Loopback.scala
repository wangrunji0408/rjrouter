package router.pipeline

import chisel3._
import chisel3.util._
import router._

// Reverse src and dst MAC then send to the origin iface.
class LoopbackPipeline extends Pipeline {
  io.out <> io.in
  when(isFirstBeat) {
    val ethIn = io.in.bits.data.asTypeOf(new EtherHeader())
    // swap src and dst MAC
    val ethOut = WireDefault(ethIn)
    ethOut.ethSrc := ethIn.ethDst
    ethOut.ethDst := ethIn.ethSrc
    io.out.bits.data := ethOut.asUInt
  }
}
