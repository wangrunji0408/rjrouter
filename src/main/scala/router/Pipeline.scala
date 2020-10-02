package router

import chisel3._
import chisel3.util.Decoupled

// The main frame processing pipeline
class Pipeline extends Module {
  val io = IO(new Bundle {
      val in = Flipped(Decoupled(new AXIStreamData(8 * 48)))
      val out = Decoupled(new AXIStreamData(8 * 48))
  })
}
