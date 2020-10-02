package router

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._
import chisel3.util.Decoupled

class AXIStreamTester extends FreeSpec with ChiselScalatestTester {

  "AXIStream should have correct input and output" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Flipped(Decoupled(new AXIStreamData(8 * 4)))
        val out = Decoupled(new AXIStreamData(8 * 4))
      })
      io.out <> io.in
    }) { dut =>
      dut.io.in.initSource()
      dut.io.in.setSourceClock(dut.clock)
      dut.io.out.initSink()
      dut.io.out.setSinkClock(dut.clock)

      val data = "0123456789abcd".getBytes()
      val datas = AXIStreamData.fromPacket(1, data, 8 * 4)

      fork {
        // push inputs into the calculator, stall for 11 cycles one third of the way
        val (seq1, seq2) = datas.splitAt(datas.length / 3)
        dut.io.in.enqueueSeq(seq1)
        dut.clock.step(11)
        dut.io.in.enqueueSeq(seq2)
      }.fork {
        // retrieve computations from the calculator, stall for 10 cycles one half of the way
        val (seq1, seq2) = datas.splitAt(datas.length / 2)
        dut.io.out.expectDequeueSeq(seq1)
        dut.clock.step(10)
        dut.io.out.expectDequeueSeq(seq2)
      }.join()

    }
  }
}
