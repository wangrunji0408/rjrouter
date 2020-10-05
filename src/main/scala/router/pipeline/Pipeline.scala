// Definations of pipeline

package router.pipeline

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import router._
import router.table.ArpModify

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
  // Generate keep literal
  def keep(n: Int, wb: Int) = (((BigInt(1) << n) - 1) << (wb - n)).U(wb.W)

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
            _.keep -> keep(data.length, wb),
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
    val data = datas.flatMap(_.litToBytes)
    (id, data)
  }
}

class PipelineBundle extends Bundle {
  val in = Flipped(Decoupled(new AXIStreamData(48 * 8)))
  val out = Decoupled(new AXIStreamData(48 * 8))
  val config = Input(new RouterConfig())
  val arpModify = new ArpModify
}

class Pipeline extends Module {
  val io = IO(new PipelineBundle())

  val isFirstBeat = RegInit(true.B)
  when(io.in.fire) {
    isFirstBeat := io.in.bits.last
  }

  // drop current and rest beat?
  private val drop = RegInit(false.B)
  // auto reset after last beat
  when(io.in.fire && io.in.bits.last) {
    drop := false.B
  }
  // for implementor
  def dropRest() {
    out.valid := false.B
    out.bits := DontCare
    drop := !io.in.bits.last
  }

  // output for implementor
  val out = Wire(Valid(new AXIStreamData))
  out.valid := io.in.valid
  out.bits := io.in.bits

  // default output
  io.in.ready := io.out.ready || drop
  io.out.valid := RegNext(out.valid && !drop)
  io.out.bits := RegNext(out.bits)
  io.arpModify.setNone()
}
