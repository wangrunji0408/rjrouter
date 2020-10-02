package router

import org.pcap4j.core._;
import java.io.EOFException

object RouterTest extends App {
    val handle = Pcaps.openOffline("src/test/resources/in_frames.pcap")
    try {
        while (true) {
            val packet = handle.getNextPacketEx()
            Console.println(packet)
            Console.println(packet.getPayload())
        }
    } catch {
        case e: EOFException => {}
    }
    handle.close()
}
