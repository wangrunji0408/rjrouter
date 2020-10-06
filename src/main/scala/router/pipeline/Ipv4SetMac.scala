package router.pipeline

import chisel3._
import chisel3.util._
import router._

// Query ARP cache and set ethernet header.
class Ipv4SetMac extends Pipeline(hasArpQuery = true) {

  when(io.in.valid && isFirstBeat) {
    val ethIn = io.in.bits.data.asTypeOf(new EtherHeader())
    val ipv4In = ethIn.payload.asTypeOf(new Ipv4Header())
    val nextHop = ethIn.ethDst.asTypeOf(new NextHop())
    when(ethIn.ethType === EthType.IPV4) {
      // debug print
      // printf(p"$ipv4In checksum=${ipv4In.calcChecksum()}\n")
      // when(ipv4In.protocol === IpProtocol.UDP) {
      //   val udpIn = ipv4In.payload.asTypeOf(new UdpHeader())
      //   printf(p"  $udpIn\n")
      // }

      io.arpQuery.get.ipv4 := nextHop.nextHop
      val dstMac = io.arpQuery.get.mac
      when(dstMac.valid) {
        // update ethernet header
        val ethOut = WireInit(ethIn)
        ethOut.ethSrc := io.config.mac(nextHop.iface)
        ethOut.ethDst := dstMac.bits

        // output
        out.valid := true.B
        out.bits.id := nextHop.iface
        out.bits.data := ethOut.asUInt
      }.otherwise {
        // TODO
        dropRest()
      }
    }
  }
}
