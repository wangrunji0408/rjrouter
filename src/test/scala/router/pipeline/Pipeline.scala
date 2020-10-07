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

class RouterTester extends PipelineTester {
  "Router generate output" in {
    test(new Router()) { dut =>
      initAndInput(dut, "src/test/resources/test_in.pcap")
        .fork {
          dumpOutput(dut, "src/test/resources/test_out.pcap")
        }
        .join()
    }
  }
}

class LoopbackTester extends PipelineTester {
  "Loopback generate output" in {
    test(new LoopbackPipeline()) { dut =>
      initAndInput(dut, "src/test/resources/test_in.pcap")
        .fork {
          dumpOutput(dut, "src/test/resources/loopback_out.pcap")
        }
        .join()
    }
  }
}

class ArpPipelineTester extends PipelineTester {
  "ArpPipeline should works" in {
    test(new ArpPipeline()) { dut =>
      initAndInput(dut, "src/test/resources/arp_in.pcap")
        // .fork {
        //   dumpOutput(dut, "src/test/resources/arp_out.pcap")
        // }
        .fork {
          val output = loadAxisFromPcap("src/test/resources/arp_ans.pcap")
          dut.io.out.expectDequeueSeq(output)
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
      initAndInput(dut, "src/test/resources/test_in.pcap")
        .fork {
          val output = loadAxisFromPcap("src/test/resources/test_in.pcap")
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
    for ((iface, i) <- dut.io.config.iface.zipWithIndex) {
      iface.ipv4.poke(Ipv4Addr(s"10.0.$i.1"))
      iface.mask.poke(Ipv4Addr("255.255.255.0"))
      iface.mac.poke(MacAddr(s"RJGG_$i".map(_.toByte).toArray))
    }
    fork {
      val input = loadAxisFromPcap(filePath)
      for (elt <- input) {
        dut.io.in.enqueue(elt)
        dut.clock.step(5)
      }
    }
  }

  def dumpOutput(dut: Pipeline, filePath: String) = {
    val buf = ArrayBuffer[Byte]()
    val packets = ArrayBuffer[(Int, Array[Byte])]()
    var end = false
    while (!end) {
      dut.io.out.ready.poke(true.B)
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
      dut.io.out.ready.poke(false.B)
      dut.clock.step(5)
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
      Pcaps.openOffline("src/test/resources/test_in.pcap").dumpOpen(filePath)
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
