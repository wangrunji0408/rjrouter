package router.pipeline

import chisel3._
import chisel3.util._
import router._
import router.table._

class RouterPipeline extends Pipeline {
  val arp = Module(new ArpPipeline)
  val ipv4 = Module(new Ipv4Pipeline)
  val arpCache = Module(new ArpCache)

  arp.io.config <> io.config
  ipv4.io.config <> io.config

  io.in <> arp.io.in
  arp.io.out <> ipv4.io.in
  ipv4.io.out <> io.out

  arp.io.arpModify <> arpCache.io.modify
  arp.io.arpQuery := DontCare
  ipv4.io.arpQuery <> arpCache.io.query
  io.arpQuery := DontCare
}
