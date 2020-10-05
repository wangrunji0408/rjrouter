package router.table

import chisel3._
import chisel3.util._
import router._

class ArpQuery extends Bundle {
  val ipv4 = Output(new Ipv4Addr)
  val mac = Input(Valid(new MacAddr))

  def setNone() = {
    ipv4 := Ipv4Addr("0.0.0.0")
  }
}

class ArpModify extends Bundle {
  val op = Output(UInt(3.W))
  val ipv4 = Output(new Ipv4Addr)
  val mac = Output(new MacAddr)

  def setNone() = {
    op := ArpOp.None
    ipv4 := DontCare
    mac := DontCare
  }
}

object ArpOp {
  val None = 0.U
  val Insert = 1.U
  val Update = 2.U
  val Remove = 3.U
  val Clear = 4.U
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
    val mac = new MacAddr
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
  io.query.mac.valid := entry.valid && entry.bits.ipv4 === key
  io.query.mac.bits := entry.bits.mac

  val idx = hashIndex(io.modify.ipv4)
  when(io.modify.op === ArpOp.Insert) {
    table(idx).valid := true.B
    table(idx).bits.ipv4 := io.modify.ipv4
    table(idx).bits.mac := io.modify.mac
  }
  when(io.modify.op === ArpOp.Update) {
    when(table(idx).valid && table(idx).bits.ipv4 === io.modify.ipv4) {
      table(idx).bits.mac := io.modify.mac
    }
  }
  when(io.modify.op === ArpOp.Remove) {
    when(table(idx).bits.ipv4 === io.modify.ipv4) {
      table(idx).valid := false.B
    }
  }
}
