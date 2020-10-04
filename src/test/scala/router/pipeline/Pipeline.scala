package router.pipeline

import org.pcap4j.core._;
import java.io.File
import java.io.EOFException
import java.nio.ByteBuffer
import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.util.Decoupled
import scala.collection.mutable.ArrayBuffer
import router._

class LoopbackTester extends PipelineTester {
  "Loopback generate output" in {
    test(new LoopbackPipeline()) { dut =>
      initAndInput(dut, "src/test/resources/in.pcap")
        .fork {
          dumpOutput(dut, "src/test/resources/loopback_out.pcap")
        }
        .join()
    }
  }
}

class ArpRequestTester extends PipelineTester {
  "ArpRequest generate output" in {
    test(new ArpRequest()) { dut =>
      initAndInput(dut, "src/test/resources/in.pcap")
        .fork {
          dumpOutput(dut, "src/test/resources/arp_request_out.pcap")
        }
        .join()
    }
  }
}

class Ipv4PipelineTester extends PipelineTester {
  "Ipv4Pipeline generate output" in {
    test(new Ipv4Pipeline()) { dut =>
      initAndInput(dut, "src/test/resources/in.pcap")
        .fork {
          dumpOutput(dut, "src/test/resources/ipv4_out.pcap")
        }
        .join()
    }
  }
}

class NopTester extends PipelineTester {
  "Nop should works" in {
    test(new Pipeline {
      io.out <> io.in
    }) { dut =>
      initAndInput(dut, "src/test/resources/in.pcap")
        .fork {
          val output = loadAxisFromPcap("src/test/resources/stdout.pcap")
          dut.io.out.expectDequeueSeq(output)
        }
        .join()
    }
  }
}

class PipelineTester extends FreeSpec with ChiselScalatestTester {

  def initAndInput(dut: Pipeline, filePath: String) = {
    dut.io.in.initSource().setSourceClock(dut.clock)
    dut.io.out.initSink().setSinkClock(dut.clock)
    for (i <- 0 until 4) {
      dut.io.config.ipv4(i).poke(Ipv4Addr(s"10.0.$i.1"))
      dut.io.config.mac(i).poke(MacAddr(s"RJGG_$i".map(_.toByte).toArray))
    }
    fork {
      val input = loadAxisFromPcap(filePath)
      dut.io.in.enqueueSeq(input)
    }
  }

  def dumpOutput(dut: Pipeline, filePath: String) = {
    val buf = ArrayBuffer[Byte]()
    val packets = ArrayBuffer[(Int, Array[Byte])]()
    var end = false
    dut.io.out.ready.poke(true.B)
    while (!end) {
      fork
        .withRegion(Monitor) {
          // wait for valid
          var count = 0
          val timeout = 100
          while (
            dut.io.out.valid.peek().litToBoolean == false && count < timeout
          ) {
            dut.clock.step(1)
            count += 1
          }
          if (count == timeout) {
            end = true
          } else {
            val bits = dut.io.out.bits.peek()
            val id = bits.id.litValue.toInt
            val last = bits.last.litToBoolean
            val data = bits.litToBytes
            buf ++= data
            if (last) {
              // Console.println(id, buf.map { b => f"$b%02x" }.mkString(" "))
              packets += ((id, buf.toArray[Byte]))
              buf.clear()
            }
          }
        }
        .joinAndStep(dut.clock)
    }
    storePackets(filePath, packets)
  }

  def loadAxisFromPcap(filePath: String): Seq[AXIStreamData] =
    loadPackets(filePath).flatMap {
      case (id, data) => AXIStreamData.fromPacket(id, data, 8 * 48)
    }

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
