package router

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

// The input and output of frame datapath.
class AXIStreamData(val w: Int) extends Bundle {
  // transmited payload. only valid when both `valid` and `ready` is true.
  val data = Bits(w.W)
  // valid for each bytes in the last cycle. only valid when `last` is true.
  val keep = Bits((w / 8).W)
  // is the last cycle?
  val last = Bool()
  // iface ID of the packet
  val id = UInt(3.W)

  def litToBytes(): Array[Byte] = {
    if (last.litToBoolean) {
      data.litValue().toByteArray.slice(0, keep.litValue().bitCount)
    } else {
      data.litValue().toByteArray
    }
  }

  def ethSrc(): UInt = data(6 * 8 - 1, 0)
  def ethDst(): UInt = data(12 * 8 - 1, 6 * 8)
  def ethType(): UInt = data(14 * 8 - 1, 12 * 8)
}

object AXIStreamData {
  def fromPacket(
      id: Int,
      packet: Array[Byte],
      width: Int
  ): Array[AXIStreamData] = {
    packet
      .grouped(width / 8)
      .zipWithIndex
      .map {
        case (data, i) =>
          (new AXIStreamData(width)).Lit(
            _.data -> BigInt(Array(0.toByte) ++ data.reverse).U(width.W),
            _.keep -> ((BigInt(1) << data.length) - 1).U,
            _.last -> ((i + 1) * width / 8 >= packet.length).B,
            _.id -> id.U
          )
      }
      .toArray
  }

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
