package router

import chisel3._
import chisel3.util._

// Reverse src and dst MAC then send to the origin iface.
class LoopbackPipeline extends PipelineModule {

  val isFirstBeat = RegInit(true.B)
  when(io.in.valid) {
    isFirstBeat := io.in.bits.last
  }

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
