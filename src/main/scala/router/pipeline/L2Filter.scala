package router.pipeline

import chisel3._
import router._

// Filter out packets with the wrong destination MAC address or the wrong type
class L2Filter extends Pipeline {

  when(io.in.valid && isFirstBeat) {
    val ethIn = io.in.bits.data.asTypeOf(new EtherHeader())
    val dstIsMe = ethIn.ethDst === io.config.iface(io.in.bits.id).mac
    when(!(dstIsMe || ethIn.ethDst === MacAddr.BROADCAST)) {
      dropRest()
    }
    when(!(ethIn.ethType === EthType.ARP || ethIn.ethType === EthType.IPV4)) {
      dropRest()
    }
  }
}
