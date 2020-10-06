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
        val iface = io.config.iface(id)
        val targetIsMe = arpIn.dstIpv4 === iface.ipv4
        // update ARP cache
        io.arpModify.get.op := Mux(targetIsMe, ArpOp.Insert, ArpOp.Update)
        io.arpModify.get.ipv4 := arpIn.srcIpv4
        io.arpModify.get.mac := arpIn.srcMac

        when(targetIsMe && arpIn.opcode === ArpOpcode.Request) {
          val ethOut = WireInit(ethIn)
          val arpOut = WireInit(arpIn)
          // construct reply
          ethOut.ethSrc := iface.mac
          ethOut.ethDst := ethIn.ethSrc
          arpOut.opcode := ArpOpcode.Response
          arpOut.srcMac := iface.mac
          arpOut.dstMac := ethIn.ethSrc
          arpOut.srcIpv4 := iface.ipv4
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
