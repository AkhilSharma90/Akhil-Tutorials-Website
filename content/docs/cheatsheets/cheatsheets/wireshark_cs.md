---
title: Wireshark Cheat sheet
date: 2023-12-18 15:01:35 +0300
image: 'https://wallpaperaccess.com/full/5927932.jpg'
draft: false
---

Protocols - ether, fddi, ip, arp, rarp, decnet, lat, sca, moprc, mopdl, tcp and udp
---

# Wireshark Capturing Modes

| Promiscuous mode | Sets interface to capture all packets on a network segment to which it is associated to |
| --- | --- |
| Monitor mode | setup the Wireless interface to capture all traffic it can receive (Unix/Linux only) |

# **Filter Types**

| Capture filter | Filter packets during capture |
| --- | --- |
| Display Filter | Hide Packets from a capture display |

# **Capture Filter Syntax**

| Syntax | protocol | direction | hosts | value | Logical operator | Expressions |
| --- | --- | --- | --- | --- | --- | --- |
| Example | tcp | src | 192.168.1.1 | 80 | and | tcp dst 202.164.30.1 |

# **Display Filter Syntax**

| Syntax | protocol | String 1 | String 2 | Comparison Operator | value | logical operator | Expressions |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Example | http | dest | ip | == | 192.168.1.1 | and | tcp port |

# Protocols - Values

---

ether, fddi, ip, arp, rarp, decnet, lat, sca, moprc, mopdl, tcp and udp

---

# **Filtering packets (Display Filters)**

| Operator | Description | Example |
| --- | --- | --- |
| eq or == | Equal | ip.dest == 192.168.1.1 |
| ne or != | Not Equal | ip.dest != 192.168.1.1 |
| gt or > | Greater than | frame.len > 10 |
| lt or < | Less than | frame.len <10 |
| ge or >= | Greater than or Equal | frame.len >= 10 |
| le or <= | Less than or Equal | frame.len<=10 |

# Miscellaneous

| Slice Operator | […] - Range of values |
| --- | --- |
| Membership Operator | {} - In |
| CTRL+E - | Start/Stop Capturing |

# **Logical Operators**

| Operator | Description | Example |
| --- | --- | --- |
| and or && | Logical AND | All the conditions should match |
| or or || | Logical OR | Either all or one of the condition should match |
| xor or ^^ | Logical XOR | exclusive alternation – Only one of the two conditions should match not both |
| not or ! | NOT(Negation) | Not equal to |
| [n] […] | Substring operator | Filter a specific word or text |

# **Default columns in a packet capture output**

| No. | Frame number from the beginning of the packet capture |
| --- | --- |
| Time | Seconds from the first frame |
| Source (src) | Source address, commonly an IPv4, IPv6 or Ethernet address |
| Destination (dst) | Destination address |
| Protocol | Protocol used in the Ethernet frame, IP packet, or TCP segment |
| Length | Length of the frame in bytes |

# **Keyboard Shortcuts**

| Accelerator | Description | Accelerator | Description |
| --- | --- | --- | --- |
| Tab or Shift+Tab | Move between screen elements, e.g. from the toolbars to the packet list to the packet detail. | Alt+→ or Option+→ | Move to the next packet in the selection history. |
| ↓ | Move to the next packet or detail item. | → | In the packet detail, opens the selected tree item. |
| ↑ | Move to the previous packet or detail item. | Shift+→ | In the packet detail, opens the selected tree item and all of its subtrees. |
| Ctrl+ ↓ or F8 | Move to the next packet, even if the packet list isn’t focused. | Ctrl+→ | In the packet detail, opens all tree items. |
| Ctrl+ ↑ or F7 | Move to the previous packet, even if the packet list isn’t focused. | Ctrl+← | In the packet detail, closes all tree items. |
| Ctrl+. | Move to the next packet of the conversation (TCP, UDP or IP). | Backspace | In the packet detail, jumps to the parent node. |
| Ctrl+, | Move to the previous packet of the conversation (TCP, UDP or IP). | Return or Enter | In the packet detail, toggles the selected tree item. |

# **Common Filtering Commands**

| Usage | Filter syntax |
| --- | --- |
| Wireshark Filter by IP | ip.addr == 10.10.50.1 |
| Filter by Destination IP | ip.dest == 10.10.50.1 |
| Filter by Source IP | ip.src == 10.10.50.1 |
| Filter by IP range | ip.addr >= 10.10.50.1 and ip.addr <= 10.10.50.100 |
| Filter by Multiple Ips | ip.addr == 10.10.50.1 and ip.addr == 10.10.50.100 |
| Filter out/ Exclude IP address | !(ip.addr == 10.10.50.1) |
| Filter IP subnet | ip.addr == 10.10.50.1/24 |
| Filter by multiple specified IP subnets | ip.addr == 10.10.50.1/24 and ip.addr == 10.10.51.1/24 |
| Filter by Protocol | • dns
• http
• ftp
• ssh
• arp
• telnet
• icmp |
| Filter by port (TCP) | tcp.port == 25 |
| Filter by destination port (TCP) | tcp.dstport == 23 |
| Filter by ip address and port | ip.addr == 10.10.50.1 and Tcp.port == 25 |
| Filter by URL | http.host == “host name” |
| Filter by time stamp | frame.time >= “June 02, 2019 18:04:00” |
| Filter SYN flag | tcp.flags.syn == 1
tcp.flags.syn == 1 and tcp.flags.ack == 0 |
| Wireshark Beacon Filter | wlan.fc.type_subtype = 0x08 |
| Wireshark broadcast filter | eth.dst == ff:ff:ff:ff:ff:ff |
| WiresharkMulticast filter | (eth.dst[0] & 1) |
| Host name filter | ip.host = hostname |
| MAC address filter | eth.addr == 00:70:f4:23:18:c4 |
| RST flag filter | tcp.flags.reset == 1 |

# **Main Toolbar Items**

![Untitled](https://prod-files-secure.s3.us-west-2.amazonaws.com/66350b40-6562-46a3-a8b9-99bfce0a99ba/b112feb8-1b4b-45b8-8052-5b1c5a21d674/Untitled.webp)

[Untitled Database](https://www.notion.so/cd40c5f1508c41d2b29f8c1637ac26aa?pvs=21)


