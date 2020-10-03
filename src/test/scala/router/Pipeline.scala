package router

import org.pcap4j.core._;
import java.io.File
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
      dut.io.in.initSource().setSourceClock(dut.clock)
      dut.io.out.initSink().setSinkClock(dut.clock)

      val input =
        PipelineTester
          .loadPackets("src/test/resources/in.pcap")
          .flatMap {
            case (id, data) =>
              AXIStreamData.fromPacket(id, data, 8 * 48)
          }
      val output =
        PipelineTester
          .loadPackets("src/test/resources/stdout.pcap")
          .flatMap {
            case (id, data) =>
              AXIStreamData.fromPacket(id, data, 8 * 48)
          }

      var end = false
      fork {
        // poke input
        dut.io.in.enqueueSeq(input)
      }.fork {
        // expect output
        dut.io.out.expectDequeueSeq(output)
        end = true
      }.fork {
        // dump output to file
        val buf = ArrayBuffer[Byte]()
        val packets = ArrayBuffer[(Int, Array[Byte])]()
        while (!end) {
          fork
            .withRegion(Monitor) {
              // wait for valid
              while (dut.io.out.valid.peek().litToBoolean == false && !end) {
                dut.clock.step(1)
              }
              val bits = dut.io.out.bits.peek()
              val id = bits.id.litValue.toInt
              val last = bits.last.litToBoolean
              val data = bits.litToBytes.reverse
              buf ++= data
              if (last) {
                // Console.println(id, buf.map { b => f"$b%02x" }.mkString(" "))
                packets += ((id, buf.toArray[Byte]))
                buf.clear()
              }
            }
            .joinAndStep(dut.clock)
        }
        PipelineTester.storePackets("src/test/resources/out.pcap", packets)
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

  def storePackets(filePath: String, packets: Seq[(Int, Array[Byte])]) {
    val handle =
      Pcaps.openOffline("src/test/resources/in.pcap").dumpOpen(filePath)
    for ((id, data) <- packets) {
      val (head, tail) = data.splitAt(12)
      val buf = ByteBuffer
        .allocate(data.length + 4)
        .put(head)
        .putShort(0x8100.toShort)
        .putShort((1000 + id).toShort)
        .put(tail)
      handle.dumpRaw(buf.array())
    }
    handle.close()
  }
}
