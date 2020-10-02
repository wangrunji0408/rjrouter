package router

import chisel3._
import chisel3.util.Decoupled

// Reverse src and dst MAC then send to the origin iface.
class LoopbackPipeline extends Module {
  val io = IO(new PipelineBundle())

  val isFirstBeat = RegNext(io.in.bits.last, init = true.B)

  io.out <> io.in
  when (isFirstBeat) {
    // swap src and dst MAC
    io.out.bits.ethSrc := io.in.bits.ethDst
    io.out.bits.ethDst := io.in.bits.ethSrc
  }
}
