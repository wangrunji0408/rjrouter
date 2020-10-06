package router.pipeline

import chisel3._
import chisel3.util._
import router._
import router.table._

// Process ARP packet
class ArpPipeline extends Pipeline(hasArpModify = true) {

  when(io.in.valid && isFirstBeat) {
    val ethIn = io.in.bits.data.asTypeOf(new EtherHeader())
    val arpIn = ethIn.payload.asTypeOf(new ArpHeader())
    when(ethIn.ethType === EthType.ARP) {
      // by default drop this frame
      dropRest()

      when(arpIn.isValid) {
        val id = io.in.bits.id
        val targetIsMe = arpIn.dstIpv4 === io.config.ipv4(id)
        // update ARP cache
        io.arpModify.get.op := Mux(targetIsMe, ArpOp.Insert, ArpOp.Update)
        io.arpModify.get.ipv4 := arpIn.srcIpv4
        io.arpModify.get.mac := arpIn.srcMac

        when(targetIsMe && arpIn.opcode === ArpOpcode.Request) {
          val ethOut = WireInit(ethIn)
          val arpOut = WireInit(arpIn)
          // construct reply
          ethOut.ethSrc := io.config.mac(id)
          ethOut.ethDst := ethIn.ethSrc
          arpOut.opcode := ArpOpcode.Response
          arpOut.srcMac := io.config.mac(id)
          arpOut.dstMac := ethIn.ethSrc
          arpOut.srcIpv4 := io.config.ipv4(id)
          arpOut.dstIpv4 := arpIn.srcIpv4
          arpOut.payload := 0.U
          // output reply
          val ARP_LEN = 42
          ethOut.payload := arpOut.asUInt
          out.valid := true.B
          out.bits.id := io.in.bits.id
          out.bits.data := ethOut.asUInt
          out.bits.keep := AXIStreamData.keep(ARP_LEN, 48)
          out.bits.last := true.B
        }
      }
    }
  }
}
