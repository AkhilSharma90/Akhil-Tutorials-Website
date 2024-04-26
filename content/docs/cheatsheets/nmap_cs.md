---
description: 
linkTitle: NMAP
title: Nmap cheat sheet
weight: 10
---

### Target Specification

1. `nmap [target]` - Scan a single IP or hostname.
2. `nmap [target1,target2,etc.]` - Scan multiple targets.
3. `nmap -iL [list.txt]` - Scan targets from a list in a file.
4. `nmap [range of IP addresses]` - Scan a range of IPs.
5. `nmap [IP address/cidr]` - Scan a network using CIDR notation.
6. `nmap -iR [number]` - Scan random hosts.
7. `nmap [targets] --exclude [targets]` - Exclude listed hosts.
8. `nmap [targets] --excludefile [list.txt]` - Exclude targets from a file.

### Host Discovery

1. `nmap -sP [target]` - Ping scan (no port scan).
2. `nmap -PS [target]` - TCP SYN ping.
3. `nmap -PA [target]` - TCP ACK ping.
4. `nmap -PU [target]` - UDP ping.
5. `nmap -PE [target]` - ICMP echo request ping.
6. `nmap -PP [target]` - ICMP timestamp request ping.
7. `nmap -PM [target]` - ICMP address mask request ping.
8. `nmap -PO [target]` - IP protocol ping.
9. `nmap -PR [target]` - ARP ping (local network only).

### Scan Techniques

1. `nmap -sS [target]` - TCP SYN scan (default).
2. `nmap -sT [target]` - TCP connect scan.
3. `nmap -sU [target]` - UDP scan.
4. `nmap -sA [target]` - TCP ACK scan.
5. `nmap -sW [target]` - TCP Window scan.
6. `nmap -sM [target]` - TCP Maimon scan.
7. `nmap -sN [target]` - TCP Null scan.
8. `nmap -sF [target]` - TCP FIN scan.
9. `nmap -sX [target]` - TCP Xmas scan.
10. `nmap -sO [target]` - IP protocol scan.

### Service and Version Detection

1. `nmap -sV [target]` - Probe open ports to determine service/version info.
2. `nmap -sV --version-intensity [0-9] [target]` - Set intensity level of version detection.
3. `nmap -sV --version-light [target]` - Enable light mode for version scanning.
4. `nmap -sV --version-all [target]` - Enable intense mode for version scanning.

### OS Detection

1. `nmap -O [target]` - Enable OS detection.
2. `nmap -O --osscan-limit [target]` - Limit OS detection to confirmed open ports.
3. `nmap -O --osscan-guess [target]` - Guess more aggressively about OS detection.
4. `nmap -O --max-os-tries [number] [target]` - Set the maximum number of OS detection tries.

### Timing and Performance

1. `nmap -T0 [target]` - Paranoid (IDS evasion).
2. `nmap -T1 [target]` - Sneaky (IDS evasion).
3. `nmap -T2 [target]` - Polite (slows down the scan).
4. `nmap -T3 [target]` - Normal (default speed).
5. `nmap -T4 [target]` - Aggressive (speeds scans).
6. `nmap -T5 [target]` - Insane (fastest scans).

### Nmap Scripting Engine (NSE)

1. `nmap --script [script.nse] [target]` - Execute specific NSE script.
2. `nmap --script [category] [target]` - Execute scripts in a specific category.
3. `nmap --script "not intrusive" [target]` - Execute default scripts excluding intrusive ones.

### Firewall/IDS Evasion and Spoofing

1. `nmap -f [target]` - Fragment packets to evade firewalls.
2. `nmap --mtu [MTU] [target]` - Specify a custom MTU size.
3. `nmap -D RND:[number] [target]` - Randomize decoy addresses.
4. `nmap -S [IP] [target]` - Spoof source address.
5. `nmap -e [interface] [target]` - Use specified network interface.
6. `nmap -g [port number] [target]` - Use specified source.
7. `nmap --source-port [port number] [target]` - Use given source port.
8. `nmap --data-length [number] [target]`- Append random data to packets.
9. `nmap --randomize-hosts [target]` - Randomize target scanning order.
10. `nmap --spoof-mac [MAC|0|vendor] [target]`- Spoof MAC address. ``
11. `nmap --badsum [target]` - Generate packets with a bad checksum.

### Output Options

1. `nmap -oN [file] [target]` - Normal output to a file.
2. `nmap -oX [file] [target]` - XML output to a file.
3. `nmap -oG [file] [target]` - Grepable output to a file.
4. `nmap -oA [path/filename] [target]` - Output in all formats.
5. `nmap --open [target]` - Show only open ports.
6. `nmap --packet-trace [target]` - Show all packets sent and received.
7. `nmap --iflist` - List interfaces and routes.
8. `nmap --resume [file]` - Resume an interrupted scan.
9. `nmap --stylesheet [path] [target]` - Apply XSL stylesheet to XML output.
10. `nmap --webxml` - Use default [Nmap.org](http://nmap.org/) stylesheet for XML.
11. `oN [file]`: Standard Nmap output to a file.
12. `oG [file]`: Greppable format output to a file.
13. `oX [file]`: XML format output to a file.
14. `oA [path/filename]`: Generate Nmap, Greppable, and XML output files using basename for files.

### Miscellaneous Options

1. `nmap -6 [target]` - Enable IPv6 scanning.
2. `nmap --datadir [directory]` - Specify custom Nmap data file location.
3. `nmap --send-eth/--send-ip [target]` - Send packets using raw IP packets or Ethernet frames.
4. `nmap --privileged` - Assume that the user is fully privileged.
5. `nmap --unprivileged` - Assume the user lacks raw socket privileges.

### Port Specification and Scan Order

1. `p <port1>-<port2>`: Scans a port range.
2. `p <port1>,<port2>,...`: Scans a list of ports.
3. `pU:53,U:110,T20-445`: Mix TCP and UDP.
4. `r`: Scans linearly (does not randomize ports).
5. `-top-ports <n>`: Scan the n most popular ports.
6. `p-65535`: Leaving off the initial port in range makes Nmap scan start at port 1.
7. `p-`: Leaving off the end port in range makes Nmap scan all ports.
8. `F`: Fast (limited port) scan.

### Port Status

1. Open: An application is listening for connections on this port.
2. Closed: Probes were received but no application is listening on this port.
3. Filtered: Probes were not received, indicating that they are being dropped by some kind of filtering.
4. Unfiltered: Probes were received but a state could not be established.
5. Open/Filtered: The port was filtered or open but Nmap couldn’t establish the state.
6. Closed/Filtered: The port was filtered or closed but Nmap couldn’t establish the state.

### Fine-Grained Timing Options

1. `-min-hostgroup/max-hostgroup <size>`: Parallel host scan group sizes.
2. `-min-parallelism/max-parallelism <numprobes>`: Probes parallelization.
3. `-min-rtt-timeout/max-rtttimeout/initial-rtt-timeout <time>`: Specifies probe round trip time.
4. `-max-retries <tries>`: Caps number of port scan probe retransmissions.
5. `-host-timeout <time>`: Gives up on target after this time.
6. `-scan-delay/--max-scan-delay <time>`: Adjusts delay between probes.
7. `-min-rate <number>`: Send packets no slower than this number per second.
8. `-max-rate <number>`: Send packets no faster than this number per second.

### Nmap Scripting Engine Categories

1. auth: Utilize credentials or bypass authentication on target hosts.
2. broadcast: Discover hosts by broadcasting on the local network.
3. brute: Attempt to guess passwords for a variety of protocols.
4. default: Scripts run automatically with -sC or -A.
5. discovery: Learn more information about target hosts through various methods.
6. dos: May cause denial of service conditions in target hosts.
7. exploit: Attempt to exploit target systems.
8. external: Interact with third-party systems.
9. fuzzer: Send unexpected input in network protocol fields
10. intrusive: May impact target machines in a malicious fashion.
11. malware: Look for signs of malware infection on target hosts.
12. safe: Designed not to impact target negatively.
13. version: Measure the version of software or protocols on the target hosts
14. vuln: Measure whether target systems have a known vulnerability.

### Additional Options

1. `n`: Disables reverse IP address lookups.
2. `-reason`: Displays the reason Nmap thinks that the port is open, closed, or filtered.
3. `A`: Enables several features, including OS Detection, Version Detection, Script Scanning (default), and traceroute.
4. `6`: Use IPv6 only.
5. `-reason`: Displays the reason Nmap thinks that the port is open, closed, or filtered.

### Probing Options

1. `Pn`: Don't probe (assume all hosts are up).
2. `PB`: Default probe (TCP 80, 445 & ICMP).
3. `PS<portlist>`: Check if systems are online by probing TCP ports.
4. `PE`: Use ICMP Echo Request for probing.
5. `PP`: Use ICMP Timestamp Request for probing.
6. `PM`: Use ICMP Netmask Request for probing.

### Scan Types

1. `sn`: Probe only (host discovery, not port scan).
2. `sS`: SYN Scan.
3. `sT`: TCP Connect Scan.
4. `sU`: UDP Scan.
5. `sV`: Version Scan.
6. `O`: Used for OS Detection/fingerprinting.
7. `-scanflags`: Sets a custom list of TCP using URG ACK PSH RST SYN FIN in any order.

### Timing Options

1. `T0` (Paranoid): Very slow, used for IDS evasion.
2. `T1` (Sneaky): Quite slow, used for IDS evasion.
3. `T2` (Polite): Slows down to consume less bandwidth, runs ~10 times slower than default.
4. `T3` (Normal): Default, a dynamic timing model based on target responsiveness.
5. `T4` (Aggressive): Assumes a fast and reliable network and may overwhelm targets.
6. `T5` (Insane): Very aggressive; will likely overwhelm targets or miss open ports.

### Nmap Scripting Engine (NSE) - Specific Scripts

1. `dns-zone-transfer`: Attempts a zone file (AXFR) from a DNS server.
    - `$ nmap --script dns-zonetransfer.nse --script-args dns-zonetransfer.domain=<domain> -p53 <hosts>`
2. `http-robots.txt`: Harvests robots.txt files from discovered web servers.
    - `$ nmap --script http-robots.txt <hosts>`
3. `smb-brute`: Attempts to determine valid username and password combinations via automated guessing.
    - `$ nmap --script smb-brute.nse -p445 <hosts>`
4. `smb-psexec`: Attempts to run a series of programs on the target machine, using provided credentials as script arguments.
    - `$ nmap --script smb-psexec.nse –script-args=smbuser=<username>,smbpass=<password>[,config=<config>] -p445 <hosts>`
5. `A`: Enables several features, including OS Detection, Version Detection, Script Scanning (default), and traceroute.
6. `6`: Use IPv6 only.
7. `-reason`: Displays the reason Nmap thinks that the port is open, closed, or filtered.

The full list of Nmap Scripting Engine scripts can be found at the official Nmap website: [Nmap Scripting Engine Documentation](http://nmap.org/nsedoc/).

Running individual or groups of scripts: `nmap --script=<ScriptName>|<ScriptCategory>|<ScriptDir>`

Using the list of script arguments: `nmap --script-args=<Name1=Value1,...>`

Updating the script database: `nmap --script-updatedb`

### Useful Scripts Examples

1. `dns-zone-transfer`:
    - `$ nmap --script dns-zonetransfer.nse --script-args dns-zonetransfer.domain=<domain> -p53 <hosts>`
2. `http-robots.txt`:
    - `$ nmap --script http-robots.txt <hosts>`
3. `smb-brute`:
    - `$ nmap --script smb-brute.nse -p445 <hosts>`
4. `smb-psexec`:
    - `$ nmap --script smb-psexec.nse –script-args=smbuser=<username>,smbpass=<password>[,config=<config>] -p445 <hosts>`