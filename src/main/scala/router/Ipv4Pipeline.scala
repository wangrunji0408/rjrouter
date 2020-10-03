package router

import chisel3._
import chisel3.util._

class Ipv4Pipeline extends Pipeline {
  io.out <> io.in
  when(isFirstBeat) {
    val ethIn = io.in.bits.data.asTypeOf(new EtherHeader())
    when(ethIn.ethType === EthType.IPV4) {
      val ipv4In = ethIn.payload.asTypeOf(new Ipv4Header())
      printf(p"$ipv4In\n")
      when(ipv4In.protocol === 17.U) {
        val udpIn = ipv4In.payload.asTypeOf(new UdpHeader())
        printf(p"  $udpIn\n")
      }
    }
  }
}
