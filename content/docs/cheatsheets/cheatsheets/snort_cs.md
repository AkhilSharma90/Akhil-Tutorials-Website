---
title: Snort cheat sheet
date: 2023-12-18 15:01:35 +0300
draft: false
image: 'https://wallpapercrafter.com/desktop/161536-pixel-art-pixelated-nature-landscape-pixels-digital-art-trees-cityscape-night-stars-water-blue-background-reflection.jpg'
---

### Sniffer Mode

- `v`: Verbose mode, shows packet headers.
- `e`: Display link layer headers.
- `d`: Show application layer data (payload).
- `x`: Display packets with headers in hexadecimal format.
- `q`: Run Snort in quiet mode, less output to the console.

### Packet Logger Mode

- `r`: Read and process packets from a file (playback).
- `l <directory>`: Log the packets to a directory.
- `k <mode>`: Keep data link layer information. `<mode>` can be `none`, `normal`, or `strict`.

### NIDS Mode

- `c <config file>`: Use the specified configuration file.
- `T`: Test the current Snort configuration.
- `A <mode>`: Set the alert mode (`full`, `fast`, `console`, `none`).
- `s`: Send alert messages to the syslog.
- `M <IP>`: Send SMB alerts to the specified IP address.

### Additional Commands and Options

- `i <interface>`: Listen on the specified network interface.
- `u <user>`: Run Snort under the specified user account.
- `g <group>`: Run Snort under the specified group account.
- `F <bpf file>`: Use the specified Berkley Packet Filter file.
- `t <chroot directory>`: Run Snort in a chroot jail.
- `D`: Run Snort as a daemon (background mode).

### Snort Rules Format

- Actions include `alert`, `log`, `pass`, `activate`, `dynamic`, `drop`, `reject`, `sdrop`.
- Protocols include `tcp`, `udp`, `icmp`, `ip`.

### Snort Rule Example

```css
alert tcp $EXTERNAL_NET any -> $HOME_NET 22 (msg:"Possible SSH scan"; flags:S; threshold: type threshold, track by_src, count 5, seconds 60; sid:1000001;)

```

### Tips for Writing Snort Rules

- Always start your rule with an action and protocol.
- Specify source and destination IPs and ports using `>` for direction.
- Use `msg` to define the alert message.
- Use `sid` to uniquely identify each rule.
- Use `rev` to specify the revision of the rule.

### Advanced Rule Options

- `content`: Look for specific content in the payload.
- `flags`: Check for specific TCP flags.
- `threshold`: Define thresholds for alerts to minimize false positives.

### Log and Data Management

- Use `/var/log/snort/` or your defined directory to check for logs.
- Regularly rotate and archive logs to prevent disk space issues.

### Troubleshooting

- Use `v` for a more verbose output if you are not receiving the expected results.
- Make sure your Snort rules are correctly formatted and loaded.
- Check Snort's documentation for complex rule writing.