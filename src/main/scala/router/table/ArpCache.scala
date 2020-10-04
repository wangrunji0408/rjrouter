package router.table

import chisel3._
import chisel3.util._
import router._

class ArpQuery extends Bundle {
  val ipv4 = Output(new Ipv4Addr)
  val arp = Input(Valid(new MacAddr))
}

class ArpModify extends Bundle {
  val op = Output(UInt(2.W))
  val ipv4 = Output(new Ipv4Addr)
  val arp = Output(new MacAddr)
}

object ArpOp {
  val None = 0.U(2.W)
  val Insert = 1.U(2.W)
  val Remove = 2.U(2.W)
  val Clear = 3.U(2.W)
}

// ARP Cache. Direct mapping.
class ArpCache(val SIZE_LOG2: Int = 4) extends Module {
  val io = IO(new Bundle {
    val query = Flipped(new ArpQuery)
    val modify = Flipped(new ArpModify)
  })
  val SIZE = 1 << SIZE_LOG2

  class ArpEntry extends Bundle {
    val ipv4 = new Ipv4Addr
    val arp = new MacAddr
  }
  val table = Mem(SIZE, Valid(new ArpEntry))

  def hashIndex(ipv4: Ipv4Addr) =
    ipv4
      .asTypeOf(Vec(4, UInt(8.W)))
      .reduce((a, b) => a ^ b)
      .apply(SIZE_LOG2 - 1, 0)

  // reset | clear
  when(reset.asBool || io.modify.op === ArpOp.Clear) {
    for (i <- 0 until SIZE) {
      table(i).valid := false.B
    }
  }
  // query
  val key = io.query.ipv4
  val entry = table(hashIndex(key))
  io.query.arp.valid := entry.valid && entry.bits.ipv4 === key
  io.query.arp.bits := entry.bits.arp

  val idx = hashIndex(io.modify.ipv4)
  // insert
  when(io.modify.op === ArpOp.Insert) {
    table(idx).valid := true.B
    table(idx).bits.ipv4 := io.modify.ipv4
    table(idx).bits.arp := io.modify.arp
  }
  // remove
  when(io.modify.op === ArpOp.Remove) {
    when(table(idx).bits.ipv4 === io.modify.ipv4) {
      table(idx).valid := false.B
    }
  }
}
