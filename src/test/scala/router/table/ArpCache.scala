package router.table

import chisel3._
import chisel3.tester._
import org.scalatest._
import router._

class ArpCacheTester extends FlatSpec with ChiselScalatestTester {
  behavior of "ArpCache"

  it should "works" in {
    test(new ArpCache(2)) { dut =>
      val query = dut.io.query
      val modify = dut.io.modify

      def insert(key: Ipv4Addr, value: MacAddr) = {
        modify.op.poke(ArpOp.Insert)
        modify.ipv4.poke(key)
        modify.mac.poke(value)
        dut.clock.step(1)
      }

      def remove(key: Ipv4Addr) = {
        modify.op.poke(ArpOp.Remove)
        modify.ipv4.poke(key)
        dut.clock.step(1)
      }

      def clear() = {
        modify.op.poke(ArpOp.Clear)
        dut.clock.step(1)
      }

      def check(key: Ipv4Addr, value: Option[MacAddr]) = {
        query.ipv4.poke(key)
        query.mac.valid.expect(value.isDefined.B)
        if (value.isDefined) {
          query.mac.bits.expect(value.get)
        }
      }

      // Insert
      insert(Ipv4Addr("192.168.1.1"), MacAddr("00:00:00:00:00:01"))
      check(Ipv4Addr("192.168.1.1"), Some(MacAddr("00:00:00:00:00:01")))

      // Insert override value
      insert(Ipv4Addr("192.168.1.1"), MacAddr("ff:ff:ff:ff:ff:ff"))
      check(Ipv4Addr("192.168.1.1"), Some(MacAddr("ff:ff:ff:ff:ff:ff")))

      // Insert override key
      insert(Ipv4Addr("192.168.0.0"), MacAddr("00:00:00:00:00:00"))
      check(Ipv4Addr("192.168.0.0"), Some(MacAddr("00:00:00:00:00:00")))
      check(Ipv4Addr("192.168.1.1"), None)

      // Remove same hash
      remove(Ipv4Addr("192.168.1.1"))
      check(Ipv4Addr("192.168.0.0"), Some(MacAddr("00:00:00:00:00:00")))

      // Remove
      remove(Ipv4Addr("192.168.0.0"))
      check(Ipv4Addr("192.168.0.0"), None)

      // Insert to full
      for (i <- 1 until 5) {
        insert(Ipv4Addr(s"192.168.1.$i"), MacAddr(s"00:00:00:00:00:0$i"))
      }
      for (i <- 1 until 5) {
        check(Ipv4Addr(s"192.168.1.$i"), Some(MacAddr(s"00:00:00:00:00:0$i")))
      }

      // Clear
      clear()
      for (i <- 1 until 6) {
        check(Ipv4Addr(s"192.168.1.$i"), None)
      }
    }
  }
}
