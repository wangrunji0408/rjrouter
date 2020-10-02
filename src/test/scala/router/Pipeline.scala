package router

import org.pcap4j.core._;
import java.io.EOFException
import java.nio.ByteBuffer
import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.util.Decoupled
import scala.collection.mutable.ArrayBuffer

class PipelineTester extends FreeSpec with ChiselScalatestTester {

  "Pipeline should have correct input and output" in {
    test(new Module {
      val io = IO(new PipelineBundle())
      io.out <> io.in
    }) { dut =>
      dut.io.in.initSource()
      dut.io.in.setSourceClock(dut.clock)
      dut.io.out.initSink()
      dut.io.out.setSinkClock(dut.clock)

      val input =
        PipelineTester
          .loadPackets("src/test/resources/in.pcap")
          .flatMap {
            case (id, data) =>
              AXIStreamData.fromPacket(id, data, 8 * 48)
          }
      val output =
        PipelineTester
          .loadPackets("src/test/resources/out.pcap")
          .flatMap {
            case (id, data) =>
              AXIStreamData.fromPacket(id, data, 8 * 48)
          }

      fork {
        dut.io.in.enqueueSeq(input)
      }.fork {
        dut.io.out.expectDequeueSeq(output)
      }.join()
    }
  }
}

object PipelineTester {
  def loadPackets(filePath: String): Seq[(Int, Array[Byte])] = {
    val handle = Pcaps.openOffline(filePath)
    val packets = ArrayBuffer[(Int, Array[Byte])]()
    try {
      while (true) {
        val packet = handle.getNextRawPacketEx()
        // extract Vlan ID
        val id = ByteBuffer
          .allocate(2)
          .put(packet.slice(14, 16))
          .flip
          .getShort() - 1000
        val data = packet.slice(0, 12) ++ packet.splitAt(16)._2
        packets += ((id, data))
      }
    } catch {
      case e: EOFException => {}
    }
    handle.close()
    packets
  }

  def storePackets(filePath: String, packets: Array[(Int, Array[Byte])]) {
    val handle = Pcaps.openOffline(filePath)
    for ((id, data) <- packets) {
      val (head, tail) = data.splitAt(12)
      val buf = ByteBuffer
        .allocate(data.length + 4)
        .put(head)
        .putShort(0x8100.toShort)
        .putShort((1000 + id).toShort)
        .put(tail)
      handle.sendPacket(buf.array())
    }
  }
}
