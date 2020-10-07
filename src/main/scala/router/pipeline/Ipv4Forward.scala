package router.pipeline

import chisel3._
import chisel3.util._
import router._

// Query forward table
// update IPv4 header
// set (iface, nextHop) at eth.ethDst.
class Ipv4Forward extends Pipeline {

  when(io.in.valid && isFirstBeat) {
    val ethIn = io.in.bits.data.asTypeOf(new EtherHeader())
    val ipv4In = ethIn.payload.asTypeOf(new Ipv4Header())
    when(ethIn.ethType === EthType.IPV4) {
      // TODO: query forward table

      // to me
      for (iface <- io.config.iface) {
        when(ipv4In.dst === iface.ipv4) {
          dropRest()
        }
      }

      // default route
      val outIface = WireInit(3.U)
      val nextHop = WireInit(Ipv4Addr("10.0.3.2"))
      // direct route
      for ((iface, i) <- io.config.iface.zipWithIndex) {
        when((ipv4In.dst & iface.mask) === (iface.ipv4 & iface.mask)) {
          outIface := i.U
          nextHop := ipv4In.dst
        }
      }

      // update IPv4 header
      val ipv4Out = WireInit(ipv4In)
      ipv4Out.ttl := ipv4In.ttl - 1.U
      ipv4Out.setChecksum(ipv4In.checksum +& 0x0100.U)

      // set nexthop at Eth.ethDst
      val ethDstOut = WireInit(ethIn.ethDst.asTypeOf(new NextHop))
      ethDstOut.iface := outIface
      ethDstOut.nextHop := nextHop

      // output
      val ethOut = WireInit(ethIn)
      ethOut.ethDst := ethDstOut.asTypeOf(new MacAddr)
      ethOut.payload := ipv4Out.asUInt
      out.bits.data := ethOut.asUInt
    }
  }
}

class NextHop extends Bundle {
  val iface = UInt(16.W)
  val nextHop = new Ipv4Addr
}
