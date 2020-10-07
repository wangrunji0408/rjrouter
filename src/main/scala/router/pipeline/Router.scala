package router.pipeline

import chisel3._
import chisel3.util._
import router._
import router.table._

class Router extends Pipeline {
  // config
  val config = Wire(new RouterConfig())
  for ((iface, i) <- config.iface.zipWithIndex) {
    iface.ipv4 := Ipv4Addr(s"10.0.$i.1")
    iface.mask := Ipv4Addr("255.255.255.0")
    iface.mac := MacAddr(s"RJGG_$i".map(_.toByte).toArray)
  }

  // the output of last pipeline
  var in = io.in
  def addPipeline[T <: Pipeline](m: T): T = {
    m.io.config <> config
    in <> m.io.in
    in = m.io.out
    m
  }

  val l2filter = addPipeline(Module(new L2Filter))
  val arp = addPipeline(Module(new ArpPipeline))
  val ipv4Check = addPipeline(Module(new Ipv4Check))
  val ipv4Forward = addPipeline(Module(new Ipv4Forward))
  val ipv4SetMac = addPipeline(Module(new Ipv4SetMac))
  in <> io.out

  val arpCache = Module(new ArpCache)
  arp.io.arpModify.get <> arpCache.io.modify
  ipv4SetMac.io.arpQuery.get <> arpCache.io.query
}

object Gen {
  def main(args: Array[String]): Unit = {
    chisel3.Driver.execute(args, () => new Router())
  }
}
