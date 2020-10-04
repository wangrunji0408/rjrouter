package router.pipeline

import chisel3._
import chisel3.util._
import router._
import router.table._

// Process ARP packet
class ArpPipeline extends Pipeline {
  // state
  val sIdle :: sDrop :: Nil = Enum(2)
  val state = RegInit(sIdle)

  io.out <> io.in
  io.arpModify.setNone()
  switch(state) {
    is(sIdle) {
      when(io.in.valid && isFirstBeat) {
        val ethIn = io.in.bits.data.asTypeOf(new EtherHeader())
        val arpIn = ethIn.payload.asTypeOf(new ArpHeader())
        when(ethIn.ethType === EthType.ARP && arpIn.isValid) {
          val id = io.in.bits.id
          val targetIsMe = arpIn.dstIpv4 === io.config.ipv4(id)
          // update ARP cache
          io.arpModify.op := Mux(targetIsMe, ArpOp.Insert, ArpOp.Update)
          io.arpModify.ipv4 := arpIn.srcIpv4
          io.arpModify.mac := arpIn.srcMac
          
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
            // output reply
            val ARP_LEN = 42
            ethOut.payload := arpOut.asUInt
            io.out.bits.data := ethOut.asUInt
            io.out.bits.keep := AXIStreamData.keep(ARP_LEN, 48)
            io.out.bits.last := true.B
          }
          // drop rest frame
          when(io.in.bits.last === false.B) {
            state := sDrop
          }
        }
      }
    }
    is(sDrop) {
      io.in.ready := true.B
      io.out.valid := false.B
      io.out.bits := DontCare
      when(io.in.valid && io.in.bits.last) {
        state := sIdle
      }
    }
  }
}
