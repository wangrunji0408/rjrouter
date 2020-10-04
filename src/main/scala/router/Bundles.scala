package router

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

class RouterConfig(val nIfaces: Int = 4) extends Bundle {
  val mac = Vec(nIfaces, new MacAddr)
  val ipv4 = Vec(nIfaces, new Ipv4Addr)
}

class WrapBits(w: Int) extends Bundle {
  val bits = Bits(w.W)
  def ===(that: WrapBits) = bits === that.bits
  def =/=(that: WrapBits) = bits =/= that.bits
}

class MacAddr extends WrapBits(48) {
  override def toPrintable: Printable = {
    asTypeOf(Vec(6, UInt(8.W))).reverse
      .map(b => p"${Hexadecimal(b)}")
      .reduce((a, b) => a + ":" + b)
  }
}

object MacAddr {
  def apply(s: String) =
    (new MacAddr).Lit(_.bits -> ("h" + s.replace(":", "")).U)
  def apply(bytes: Array[Byte]) =
    (new MacAddr).Lit(_.bits -> ("h" + bytes.map(b => f"$b%02x").mkString).U)
  val BROADCAST = MacAddr("ff:ff:ff:ff:ff:ff")
}

class Ipv4Addr extends WrapBits(32) {
  override def toPrintable: Printable = {
    asTypeOf(Vec(4, UInt(8.W))).reverse
      .map(b => p"$b")
      .reduce((a, b) => a + "." + b)
  }
}

object Ipv4Addr {
  def apply(s: String) =
    (new Ipv4Addr).Lit(
      _.bits -> ("h" + s.split("\\.").map(d => f"${d.toInt}%02x").mkString).U
    )
  val LOCALHOST = Ipv4Addr("127.0.0.1")
}

class EthType extends WrapBits(16) {}

object EthType {
  private def apply(s: Short) = (new EthType).Lit(_.bits -> s.U)
  val ARP = EthType(0x0806)
  val IPV4 = EthType(0x0800)
}

class EtherHeader extends Bundle {
  val ethDst = new MacAddr
  val ethSrc = new MacAddr
  val ethType = new EthType
  val payload = Bits((34 * 8).W)
}

class ArpHeader extends Bundle {
  val hardwareType = UInt(16.W)
  val protocolType = new EthType
  val hardwareSize = UInt(8.W)
  val protocolSize = UInt(8.W)
  val opcode = UInt(16.W)
  val srcMac = new MacAddr
  val srcIpv4 = new Ipv4Addr
  val dstMac = new MacAddr
  val dstIpv4 = new Ipv4Addr
  val payload = Bits((6 * 8).W)

  def isValid =
    hardwareType === 1.U &&
      protocolType === EthType.IPV4 &&
      hardwareSize === 6.U &&
      protocolSize === 4.U &&
      (opcode === 1.U || opcode === 2.U)
}

object ArpOpcode {
  val Request = 1.U
  val Response = 2.U
}

class Ipv4Header extends Bundle {
  val version = UInt(4.W)
  val headerLen = UInt(4.W)
  val dscp_ecn = UInt(8.W)
  val totalLen = UInt(16.W)
  val id = UInt(16.W)

  val flags_reserved = UInt(1.W)
  val dontFrag = UInt(1.W)
  val moreFrag = UInt(1.W)
  val fragOffset = UInt(13.W)

  val ttl = UInt(8.W)
  val protocol = UInt(8.W)
  val checksum = UInt(16.W)
  val src = new Ipv4Addr
  val dst = new Ipv4Addr

  val payload = Bits((14 * 8).W)

  def calcChecksum() =
    asTypeOf(Vec(17, UInt(16.W)))
      .slice(7, 17)
      .fold(0.U(32.W))((a, b) => a + b)
      .asTypeOf(Vec(2, UInt(16.W)))
      .reduce((a, b) => a + b)
      .tail(16)
}

class UdpHeader extends Bundle {
  val srcPort = UInt(16.W)
  val dstPort = UInt(16.W)
  val length = UInt(16.W)
  val checksum = UInt(16.W)
  val payload = Bits((6 * 8).W)
}
