package router

import chisel3._
import chisel3.util._

class ArpPipeline extends Pipeline {
  io.out <> io.in
  when(isFirstBeat) {
    val ethIn = io.in.bits.data.asTypeOf(new EtherHeader())
    when(ethIn.ethType === EthType.ARP) {
      val arpIn = ethIn.payload.asTypeOf(new ArpHeader())
      printf(p"$arpIn\n")
    }
  }
}
