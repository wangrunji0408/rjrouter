package router.pipeline

import chisel3._
import chisel3.util._
import router._

// Check IPv4 header and drop invalid frames.
class Ipv4Check extends Pipeline {

  when(io.in.valid && isFirstBeat) {
    val ethIn = io.in.bits.data.asTypeOf(new EtherHeader())
    val ipv4In = ethIn.payload.asTypeOf(new Ipv4Header())
    when(ethIn.ethType === EthType.IPV4) {
      when(!ipv4In.isValid || ipv4In.calcChecksum() =/= 0xFFFF.U) {
        dropRest()
      }
      when(ipv4In.ttl <= 1.U) {
        dropRest()
      }
    }
  }
}
