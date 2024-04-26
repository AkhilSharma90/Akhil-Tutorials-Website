---
title: TCPdump Cheatsheet
date: 2023-12-18 15:01:35 +0300
authors: [admin]
image: 'https://wallup.net/wp-content/uploads/2016/04/06/332169-digital_art-pixel_art-pixelated-pixels-water-nature-mountain-sea-rock-Sun-clouds-trees-waves-sun_rays-cliff-hill-748x421.png'
---

## Command

| Command | Description |
| --- | --- |
| -a | Converts network and broadcast addresses to names. |
| -A | Displays each packet (excluding its link level header) in ASCII. |
| -e | Prints the link-level header on each dump line. |
| -E | Decrypt IPSEC traffic by providing an encryption key. |
| -n | Avoids converting addresses (like host addresses) to names. |
| -N | Does not print domain name qualification of host names. |
| -S | Prints absolute TCP sequence numbers. |
| -t | Omits printing of timestamp on each dump line. |
| -tt | Prints unformatted timestamp on each dump line. |
| -ttt | Prints delta (micro-second resolution) between current and previous line. |
| -tttt | Prints timestamp in default format proceeded by date on each dump line. |
| -v | Provides verbose output (slightly more detailed). |
| -vv | Provides more verbose output (more detailed than -v). |
| -vvv | Provides very verbose output (even more detailed than -vv). |
| -c <count> | Exits after receiving <count> number of packets. |
| -F <file> | Uses <file> as a filter file for reading packet filters. |
| -i <interface> | Captures packets from <interface>. If not specified, tcpdump selects a default interface. |
| -r <file> | Reads packets from <file>. |
| -s <size> | Snaps the packet at <size> bytes. Default is 65535. |
| -S | Prints absolute, rather than relative, TCP sequence numbers. |
| -w <file> | Writes the raw packets to <file> instead of parsing and printing them out. |
| -x | Prints packets in hex. |
| -X | Prints packets in hex and ASCII. |

| Command | Example usage | Explanation |
| --- | --- | --- |
| -i any | tcpdump -i any | Capture from all interfaces; may require superuser (sudo/su) |
| -i eth0 | tcpdump -i eth0 | Capture from the interface eth0 |
| -c count | tcpdump -i eth0 -c 5 | Exit after receiving count (5) packets |
| -r captures.pcap | tcpdump -i eth0 -r captures.pcap | Read and analyze saved capture file captures.pcap |
| tcp | tcpdump -i eth0 tcp | Show TCP packets only |
| udp | tcpdump -i eth0 udp | Show UDP packets only |
| icmp | tcpdump -i eth0 icmp | Show ICMP packets only |
| ip | tcpdump -i eth0 ip | Show IPv4 packets only |
| ip6 | tcpdump -i eth0 ip6 | Show IPv6 packets only |
| arp | tcpdump -i eth0 arp | Show ARP packets only |
| rarp | tcpdump -i eth0 rarp | Show RARP packets only |
| slip | tcpdump -i eth0 slip | Show SLIP packets only |
| -I | tcpdump -i eth0 -I | Set interface as monitor mode |
| -K | tcpdump -i eth0 -K | Don’t verify checksum |
| -p | tcpdump -i eth0 -p | Don’t capture in promiscuous mode |

AH
ARP
BGP
CWR
DF
DHCP
DNS
ECN
ESP
FTP
GRE
HTTP
ICMP
IGMP
IMAP
IP
display link layer in hex
display in hex + ASCII
Acronyms
Authentication Header (RFC 2402)
Address Resolution Protocol (RFC 826)
Border Gateway Protocol (RFC 1771)
Congestion Window Reduced (RFC 2481)
Do not fragment flag (RFC 791)
Dynamic Host Configuration Protocol (RFC 2131)
Domain Name System (RFC 1035)
Explicit Congestion Notification (RFC 3168)
Encapsulating Security Payload (RFC 2406)
File Transfer Protocol (RFC 959)
Generic Route Encapsulation (RFC 2784)
Hypertext Transfer Protocol (RFC 1945)
Internet Control Message Protocol (RFC 792)
Internet Group Management Protocol (RFC 2236)
Internet Message Access Protocol (RFC 2060)
Internet Protocol (RFC 791)
ISAKMP Internet Sec. Assoc. & Key Mngm Proto. (RFC 7296)
L2TP
Layer 2 Tunneling Protocol (RFC 2661)
OSPF
POP3
RFC
SMTP
SSH
SSL
TCP
TLS
TFTP
TOS
UDP
Open Shortest Path First (RFC 1583)
Post Office Protocol v3 (RFC 1460)
Request for Comments
Simple Mail Transfer Protocol (RFC 821)
Secure Shell (RFC 4253)
Secure Sockets Layer (RFC 6101)
Transmission Control Protocol (RFC793)
Transport Layer Security (RFC 5246)
Trivial File Transfer Protocol (RFC 1350)
Type of Service (RFC 2474)
User Datagram Protocol (RFC 768)

| Filter expression | Explanation |
| --- | --- |
| src host 127.0.0.1 | Filter by source IP/hostname 127.0.0.1 |
| dst host 127.0.0.1 | Filter by destination IP/hostname 127.0.0.1 |
| host 127.0.0.1 | Filter by source or destination = 127.0.0.1 |
| ether src 01:23:45:AB:CD:EF | Filter by source MAC 01:23:45:AB:CD:EF |
| ether dst 01:23:45:AB:CD:EF | Filter by destination MAC 01:23:45:AB:CD:EF |
| ether host 01:23:45:AB:CD:EF | Filter by source or destination MAC 01:23:45:AB:CD:EF |
| src net 127.0.0.1 | Filter by source network location 127.0.0.1 |
| dst net 127.0.0.1 | Filter by destination network location 127.0.0.1 |
| net 127.0.0.1 | Filter by source or destination network location 127.0.0.1 |
| net 127.0.0.1/24 | Filter by source or destination network location 127.0.0.1 with the tcpdump subnet mask of length 24 |
| src port 80 | Filter by source port = 80 |
| dst port 80 | Filter by destination port = 80 |
| port 80 | Filter by source or destination port = 80 |
| src portrange 80-400 | Filter by source port value between 80 and 400 |
| dst portrange 80-400 | Filter by destination port value between 80 and 400 |
| portrange 80-400 | Filter by source or destination port value between 80 and 400 |
| ether broadcast | Filter for Ethernet broadcasts |
| ip broadcast | Filter for IPv4 broadcasts |
| ether multicast | Filter for Ethernet multicasts |
| ip multicast | Filter for IPv4 multicasts |
| ip6 multicast | Filter for IPv6 multicasts |
| ip src host mydevice | Filter by IPv4 source hostname mydevice |
| arp dst host mycar | Filter by ARP destination hostname mycar |
| rarp src host 127.0.0.1 | Filter by RARP source 127.0.0.1 |
| ip6 dst host mywatch | Filter by IPv6 destination hostname mywatch |
| tcp dst port 8000 | Filter by destination TCP port = 8000 |
| udp src portrange 1000-2000 | Filter by source TCP ports in 1000–2000 |
| sctp port 22 | Filter by source or destination port = 22 |

-A	tcpdump -i eth0 -A	Print each packet (minus its link level header) in ASCII. Handy for capturing web pages.Without -AWith -A	https://stationx.net/wp-content/uploads/2023/02/Screenshot-with-ASCII-sudo-tcpdump-twitter.jpg,https://stationx.net/wp-content/uploads/2023/02/Screenshot-without-ASCII-sudo-tcpdump-A-twitter.jpg
-D	tcpdump -D	Print the list of the network interfaces available on the system and on which tcpdump can capture packets.	https://stationx.net/wp-content/uploads/2023/02/Output-of-tcpdump-D.jpg
-e	tcpdump -i eth0 -e	Print the link-level header on each output line, such as MAC layer addresses for protocols such as Ethernet and IEEE 802.11.	
-F params.conf	tcpdump -i eth0 -F /path/to/params.conf	Use the file params.conf as input for the filter expression. (Ignore other expressions on the command line.)	
-n	tcpdump -i eth0 -n	Don't convert addresses (i.e., host addresses, port numbers, etc.) to names.	
-S	tcpdump -i eth0 -S	Print absolute, rather than relative, TCP sequence numbers. (Absolute TCP sequence numbers are longer.)	
--time-stamp-precision=tsp	tcpdump -i eth0 --time-stamp-precision=nano	When capturing, set the timestamp precision for the capture to tsp:• micro for microsecond (default)• nano for nanosecond.	
-t	tcpdump -i eth0 -t	Omit the timestamp on each output line.	
-tt	tcpdump -i eth0 -tt	Print
 the timestamp, as seconds since January 1, 1970, 00:00:00, UTC, and 
fractions of a second since that time, on each dump line.	
-ttt	tcpdump -i eth0 -ttt	Print a delta (microsecond or nanosecond resolution depending on the --time-stamp-precision option) between the current and previous line on each output line. The default is microsecond resolution.	
-tttt	tcpdump -i eth0 -tttt	Print a timestamp as hours, minutes, seconds, and fractions of a second since midnight, preceded by the date, on each dump line.	
-ttttt	tcpdump -i eth0 -ttttt	Print a delta (microsecond or nanosecond resolution depending on the --time-stamp-precision option) between the current and first line on each dump line. The default is microsecond resolution.	
-u	tcpdump -i eth0 -u	Print undecoded network file system (NFS) handles.	
-v	tcpdump -i eth0 -v	Produce verbose output.When writing to a file (-w option) and at the same time not reading from a file (-r option), report to standard error, once per second, the number of packets captured.	
-vv	tcpdump -i eth0 -vv	Additional verbose output than -v	
-vvv	tcpdump -i eth0 -vvv	Additional verbose output than -vv	
-x	tcpdump -i eth0 -x	Print the headers and data of each packet (minus its link level header) in hex.	
-xx	tcpdump -i eth0 -xx	Print the headers and data of each packet, including its link level header, in hex.	
-X	tcpdump -i eth0 -X	Print the headers and data of each packet (minus its link level header) in hex and ASCII.	
-XX	tcpdump -i eth0 -XX	Print the headers and data of each packet, including its link level header, in hex and ASCII.	

| Command | Example | Explanation |
| --- | --- | --- |
| -w captures.pcap | tcpdump -i eth0 -w captures.pcap | Output capture to a file captures.pcap |
| -d | tcpdump -i eth0 -d | Display human-readable form in standard output |
| -L | tcpdump -i eth0 -L | Display data link types for the interface |
| -q | tcpdump -i eth0 -q | Quick/quiet output. Print less protocol information, so output lines are shorter. |
| -U | tcpdump -i eth0 -U -w out.pcap | Without -w optionPrint a description of each packet's contents.With -w optionWrite each packet to the output file out.pcap in real time rather than only when the output buffer fills. |

| Operator | Syntax | Example | Description |
| --- | --- | --- | --- |
| AND | and, && | tcpdump -n src 127.0.0.1 and dst port 21 | Combine filtering options joined by “and” |
| OR | or, || | tcpdump dst 127.0.0.1 or src port 22 | Match any of the conditions joined by “or” |
| EXCEPT | not, ! | tcpdump dst 127.0.0.1 and not icmp | Negate the condition prefixed by “not” |
| LESS | less, <, (<=) | tcpdump dst host 127.0.0.1 and less 128 | Shows packets shorter than (or equal to) 128 bytes in length.< only applies to length 32, i.e., <32. |
| GREATER | greater, >, (>=) | tcpdump dst host 127.0.0.1 and greater 64 | Shows packets longer than (or equal to) 64 bytes in length.> only applies to length 32, i.e., >32. |
| EQUAL | =, == | tcpdump host 127.0.0.1 = 0 | Show packets with zero length |