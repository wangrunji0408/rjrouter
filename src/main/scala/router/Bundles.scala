package router

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

// The input and output of pipeline.
class AXIStreamData(val w: Int = 48 * 8) extends Bundle {
  private val wb = w / 8

  // transmited payload. only valid when both `valid` and `ready` is true.
  val data = Bits(w.W)
  // valid for each bytes in the last cycle. only valid when `last` is true.
  val keep = Bits(wb.W)
  // is the last cycle?
  val last = Bool()
  // iface ID of the packet
  val id = UInt(3.W)

  def litToBytes(): Seq[Byte] = {
    var bytes = data.litValue.toByteArray.toSeq
    bytes = if (bytes.length > wb) {
      bytes.slice(bytes.length - wb, bytes.length)
    } else if (bytes.length < wb) {
      Seq.fill(wb - bytes.length) { 0.toByte } ++ bytes
    } else {
      bytes
    }
    if (last.litToBoolean) {
      bytes.slice(0, keep.litValue().bitCount)
    } else {
      bytes
    }
  }
}

object AXIStreamData {
  // Convert packet to AXIStream.
  def fromPacket(
      id: Int,
      packet: Array[Byte],
      width: Int
  ): Seq[AXIStreamData] = {
    val wb = width / 8
    packet
      .grouped(wb)
      .zipWithIndex
      .map {
        case (data, i) =>
          (new AXIStreamData(width)).Lit(
            _.data -> BigInt(
              Array(0.toByte) ++ data ++ Array.fill(wb - data.length) {
                0.toByte
              }
            ).U(width.W),
            _.keep -> (((BigInt(
              1
            ) << data.length) - 1) << (wb - data.length)).U,
            _.last -> ((i + 1) * wb >= packet.length).B,
            _.id -> id.U
          )
      }
      .toSeq
  }

  // Convert AXIStream to packet.
  def toPacket(
      datas: Array[AXIStreamData]
  ): (Int, Array[Byte]) = {
    val id = datas.head.id.litValue().toInt
    val data = datas.flatMap(data => data.litToBytes())
    (id, data)
  }
}

class PipelineBundle extends Bundle {
  val in = Flipped(Decoupled(new AXIStreamData(48 * 8)))
  val out = Decoupled(new AXIStreamData(48 * 8))
}

class PipelineModule extends Module {
  val io = IO(new PipelineBundle())
}

// Packet defination
class EtherHeader extends Bundle {
  val ethDst = Bits((6 * 8).W)
  val ethSrc = Bits((6 * 8).W)
  val ethType = Bits((2 * 8).W)
  val payload = Bits((34 * 8).W)
}
