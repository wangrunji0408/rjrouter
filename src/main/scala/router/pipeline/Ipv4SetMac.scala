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

      // query ARP cache
      io.arpQuery.get.ipv4 := nextHop.nextHop
      val dstMac = io.arpQuery.get.mac

      val iface = io.config.iface(nextHop.iface)
      when(dstMac.valid) {
        // update ethernet header
        val ethOut = WireInit(ethIn)
        ethOut.ethSrc := iface.mac
        ethOut.ethDst := dstMac.bits

        // output
        out.valid := true.B
        out.bits.id := nextHop.iface
        out.bits.data := ethOut.asUInt
      }.otherwise {
        dropRest()
        // generate ARP request
        val ethOut = WireInit(ethIn)
        ethOut.ethSrc := iface.mac
        ethOut.ethDst := MacAddr.BROADCAST
        ethOut.ethType := EthType.ARP
        val arpOut = Wire(new ArpHeader)
        arpOut.setTypeSize()
        arpOut.opcode := ArpOpcode.Request
        arpOut.srcMac := iface.mac
        arpOut.dstMac := MacAddr.BROADCAST
        arpOut.srcIpv4 := iface.ipv4
        arpOut.dstIpv4 := nextHop.nextHop
        arpOut.payload := 0.U
        ethOut.payload := arpOut.asUInt

        // output
        out.valid := true.B
        out.bits.id := nextHop.iface
        out.bits.data := ethOut.asUInt
        out.bits.keep := AXIStreamData.keep(42, 48)
        out.bits.last := true.B
      }
    }
  }
}
