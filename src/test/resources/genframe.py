#!/usr/local/bin/scapy -c

# Requirements:
# sudo apt install python3-pip
# sudo pip3 install scapy
# Usage: ./gen_frame

import scapy
import struct

MAC_BROADCAST = 'ff:ff:ff:ff:ff:ff'
MAC_TESTER = '54:57:44:32:5f:5f'
MAC_TESTER2 = '54:57:44:32:5f:60'
IFACE_DEFAULT_ROUTE = 3
MAC_DEFAULT_ROUTE = '54:57:44:32:5f:61'

# You may change these IP addresses.
IP_TESTER = '10.0.0.2'
IP_TESTER2 = '10.0.1.2'
IP_DUT = '10.0.0.1'  # Device under test.
IP_DUT3 = '10.0.3.1'
IP_DEFAULT_ROUTE = '10.0.3.2'  # The IP address of the default route.
IP_TEST_DST = '192.168.1.1'  # Forward destination. Route should exist.
IP_TEST_DST_NO_MAC = '10.0.0.100'  # Forward destination. Route should exist. MAC address should not exist.
IP_TEST_DST_NO_ROUTE = '254.254.254.254'  # Forward destination. Route should not exist.

# frames.txt format:
# <Ingress Interface ID> <Frame Length> <Frame Data...>

fout = open('frames.txt', 'w')  # for simulation
pout = RawPcapWriter('in_frames.pcap', DLT_EN10MB)  # for wireshark

def write_frame(iface, f):
  print('Writing frame (interface #{}):'.format(iface))
  f.show()
  data = bytes(f)
  # We use VLAN ID to indicate the interface ID in pcap files.
  pout.write(data[:12] + struct.pack('>HH', 0x8100, 1000 + iface) + data[12:])
  fout.write('{} {} '.format(iface, len(data)))
  fout.write(' '.join(map(lambda x: '{:02x}'.format(x), data)))
  fout.write('\n')

# ARP request test.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            ARP(op='who-has', hwsrc=MAC_TESTER, psrc=IP_TESTER, hwdst=MAC_BROADCAST, pdst=IP_DUT))

# ARP reply test.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            ARP(op='is-at', hwsrc=MAC_TESTER, psrc=IP_TESTER, hwdst=MAC_BROADCAST, pdst=IP_DUT))

# Fill the ARP cache entry of the default route.
write_frame(IFACE_DEFAULT_ROUTE, Ether(src=MAC_DEFAULT_ROUTE, dst=MAC_BROADCAST)/
            ARP(op='who-has', hwsrc=MAC_DEFAULT_ROUTE, psrc=IP_DEFAULT_ROUTE, hwdst=MAC_BROADCAST, pdst=IP_DUT3))

# Simple IP forwarding test.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER, dst=IP_TEST_DST)/
            UDP(sport=7, dport=7)/b'hello, 00001')

# Packet to the router itself, should not be forwarded.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER, dst=IP_DUT)/
            UDP(sport=7, dport=7)/b'hello, 00002')

# Simple IP forwarding test (no MAC).
write_frame(1, Ether(src=MAC_TESTER2, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER2, dst=IP_TEST_DST_NO_MAC)/
            UDP(sport=7, dport=7)/b'hello, 00003')

# Simple IP forwarding test (no route).
write_frame(1, Ether(src=MAC_TESTER2, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER2, dst=IP_TEST_DST_NO_ROUTE)/
            UDP(sport=7, dport=7)/b'hello, 00004')

# TTL=2 test.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER, dst=IP_TEST_DST, ttl=2)/
            UDP(sport=7, dport=7)/b'hello, 00005')

# TTL=1 test.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER, dst=IP_TEST_DST, ttl=1)/
            UDP(sport=7, dport=7)/b'hello, 00006')

# TTL=0 test.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER, dst=IP_TEST_DST, ttl=0)/
            UDP(sport=7, dport=7)/b'hello, 00007')

# IP packet with wrong checksum test.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER, dst=IP_TEST_DST, chksum=0x1234)/
            UDP(sport=7, dport=7)/b'hello, 00008')

# IP packet with 0xfeff checksum test.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER, dst=IP_TEST_DST, id=0xb01a, ttl=64)/
            UDP(sport=7, dport=7)/b'hello, 00009')

# IP packet with 0 checksum test.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER, dst=IP_TEST_DST, id=0xaf1a, ttl=64)/
            UDP(sport=7, dport=7)/b'hello, 00010')

# IP packet with 0xffff checksum test.
write_frame(0, Ether(src=MAC_TESTER, dst=MAC_BROADCAST)/
            IP(src=IP_TESTER, dst=IP_TEST_DST, id=0xaf1a, ttl=64, chksum=0xffff)/
            UDP(sport=7, dport=7)/b'hello, 00011')

# You can construct more frames to test your datapath.

fout.close()
pout.close()
exit(0)
