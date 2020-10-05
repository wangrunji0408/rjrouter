package router.pipeline

import chisel3._
import chisel3.util._
import router._

class Ipv4Pipeline extends Pipeline {

  when(io.in.valid && isFirstBeat) {
    val ethIn = io.in.bits.data.asTypeOf(new EtherHeader())
    val ipv4In = ethIn.payload.asTypeOf(new Ipv4Header())
    when(ethIn.ethType === EthType.IPV4) {
      // debug print
      // printf(p"$ipv4In checksum=${ipv4In.calcChecksum()}\n")
      // when(ipv4In.protocol === IpProtocol.UDP) {
      //   val udpIn = ipv4In.payload.asTypeOf(new UdpHeader())
      //   printf(p"  $udpIn\n")
      // }

      // default router
      val outId = 3.U
      val nextHop = Ipv4Addr("10.0.3.2")

      io.arpQuery.ipv4 := nextHop
      val dstMac = io.arpQuery.mac
      when(dstMac.valid) {
        val ipv4Out = WireInit(ipv4In)
        val ethOut = WireInit(ethIn)
        ethOut.ethSrc := io.config.mac(outId)
        ethOut.ethDst := dstMac.bits
        ipv4Out.ttl := ipv4In.ttl - 1.U
        ipv4Out.checksum := ipv4In.checksum + 0x0100.U

        ethOut.payload := ipv4Out.asUInt
        out.valid := true.B
        out.bits.id := outId
        out.bits.data := ethOut.asUInt
      }.otherwise {
        // TODO
        dropRest()
      }
    }
  }
}
