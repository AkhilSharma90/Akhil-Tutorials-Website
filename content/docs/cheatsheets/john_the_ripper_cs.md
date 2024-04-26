---
description: 
linkTitle: John the ripper
title: John the ripper Cheat Sheet
weight: 10
---

## John Installation

```
git clone https://github.com/openwall/john -b bleeding-jumbo /data/tools/john ; cd /data/tools/john/src/ ; ./configure && make -s clean && make -sj4 ; cd ~
```

## John Modes

- Wordlist mode (dictionary attack) - `john --wordlist=<wordlist> <hash>`
- Mangling rules mode - `john --wordlist=<wordlist> --rules:<rulename> <hash>`
- Incremental mode - `john --incremental <hash>`
- External mode - `john --external:<rulename> <hash>`
- Loopback mode (use .pot files) - `john --loopback <hash>`
- Mask mode - `john --mask=?1?1?1?1?1?1?1?1 -1=[A-Z] -min-len=8 <hash>`
- Markov mode - `calc_stat <wordlist> markovstats` `john -markov:200 -max-len:12 --mkv-stats=markovstats <hash>`
- Prince mode - `john --prince=<wordlist> <hash>`

Refer the [link](https://4n3i5v74.github.io/posts/tryhackme-john-the-ripper/) for more examples.

## CPU and GPU options

- List opencl devices - `john --list=opencl-devices`
- List formats supported by opencl - `john --list=formats --format=opencl`
- Use multiple CPU - `john hashes --wordlist:<wordlist> --rules:<rulename> --dev=2 --fork=4`
- Use multiple GPU - `john hashes --format:<openclformat> --wordlist:<wordlist> --rules:<rulename> --dev=0,1 --fork=2`

## Rules

- Single
- wordlist
- Extra
- Jumbo (Single, wordlist and Extra)
- KoreLogic
- All (Single, wordlist, Extra and KoreLogic)

## Incremental modes

- Lower (26 char)
- Alpha (52 char)
- Digits (10 char)
- Alnum (62 char)

## New rule

```
[List.Rules:Tryout]
l                       [convert to lowercase]
u                       [convert to uppercase]
c                       [capitalize]
l r                     [lowercase and reverse (palindrome)]
l Az"2015"              [lowercase and append "2015" at end of word]
l A0"2015"              [lowercase and prepend "2015" at end of word]
d                       [duplicate]
A0"#"Az"#"              [append and prepend "#"]

```

- Display password candidates - `john --wordlist=<wordlist> --stdout --rules:Tryout`
- Generate password candidates - `john --wordlist=<wordlist> --stdout=8 --rules:Tryout`

## Other rules

```
C     [lowercase first char, uppercase rest]
t     [toggle case of all chars]
TN    [toggle case of char in position N]
r     [reverse word - test123 -> 321tset]
d     [duplicate word - test123 -> test123test123]
f     [reflect word - test123 -> test123321tset]
{     [rotate word left - test123 -> est123t]
}     [rotate word right - test123 -> 3test12]
$X    [append word with X]
^X    [prefix word with X]
[     [remove first char]
]     [remove last char]
DN    [delete char in posision N]
xNM   [extract from position N till M chars]
iNX   [insert X in place of N and shift rest right]
oNX   [overwrite N with X]
S     [shift case - test123 -> TEST!@#]
V     [lowercase vowels, uppercase consonents - test123 -> TeST123]
R     [shift each char right, using keyboard key - test123 -> yrdy234]
L     [shift each char left, using keyboard key - test123 -> rwar012]
<N    [reject words unless less than length N]
>N    [reject words unless greater than length N]
N     [truncate to length N]

```

## New charset

```
john --make-charset=set.char

```

Create `john.conf` with character set config.

```
# Incremental modes
[Incremental:charset]
File = $JOHN/set.char
MinLen = 0
MaxLen = 30
CharCount = 80

```

```
john --incremental=charset <hash>

```

## Wordlists

- Sort wordlist - `tr A-Z a-z < <wordlist> | sort -u > <new-wordlist>`
- Generate wordlist using POT - `cut -d: -f2 john.pot | sort -u > pot.dict`
- Generate candidate pwd for slow hash - `john --wordlist=<wordlist> --stdout --rules:Jumbo | unique -mem=25 <unique-wordlist>`

## External mode

- Create complex password list - [link](http://www.lanmaster53.com/2011/02/creating-complex-password-lists-with-john-the-ripper/)
- Generate wordlist according to complexity filter - `./john --wordlist=<wordlist> --stdout --external:<filter> > <filtered-wordlist>`
- Use adjacent keys on `keyboard` - `john --external:Keyboard <hash>`

## Misc Options

- Hidden options - `john --list=hidden-options`
- Display guesses - `john --incremental:Alpha -stdout -session=s1`
- Generate guesses with external programs - `crunch 1 6 abcdefg | ./john hashes -stdin -session=s1`
- Save session - `john hashes -session=name`
- Restore session - `john --restore:name`
- Show cracked passwords - `john hashes --pot=<pot> --show`

## Dictionaries

- Generate wordlist from wikipedia - `wget https://raw.githubusercontent.com/zombiesam/wikigen/master/wwg.py ;
python wwg.py -u http://pt.wikipedia.org/wiki/Fernando_Pessoa -t 5 -o
fernandopessoa -m3`
- Aspell dictionary - `apt-get install aspell-es` `aspell dump dicts` `aspell -d es dump master | aspell -l es expand | awk 1 RS=" |\n" > aspell.dic`

# **John The Ripper Hash Formats**

1. **afs – Kerberos AFS DES**: AFS (Andrew File System) uses Kerberos for authentication. The DES (Data Encryption Standard) is used for the encryption of Kerberos tickets.
2. **bfegg – Eggdrop**: Eggdrop is an IRC bot software, and bfegg is the format used for storing user passwords in Eggdrop using Blowfish encryption.
3. **bf – OpenBSD Blowfish**: This is a Blowfish-based password hashing method, commonly used in OpenBSD for encrypting passwords.
4. **bsdi – BSDI DES**: A format used by BSDI operating systems for password hashing, based on the DES algorithm.
5. **crypt – generic crypt(3)**: A generic format for the Unix crypt(3) function, which can support various hashing algorithms.
6. **des – Traditional DES**: The traditional DES (Data Encryption Standard) format used for Unix passwords.
7. **dmd5 – DIGEST-MD5**: A challenge-response scheme based on MD5 used in HTTP and other protocols for authentication.
8. **dominosec – More Secure Internet Password**: Used by Lotus Domino for password hashing.
9. **EPiServer SID Hashes**: EPiServer uses a specific format for hashing, but there's no specific format flag in JtR.
10. **hdaa – HTTP Digest access authentication**: Used in HTTP for digest access authentication.
11. **hmac-md5 – HMAC MD5**: A format using HMAC (Hash-based Message Authentication Code) with MD5 hashing.
12. **hmailserver – hmailserver**: A format used by hMailServer, an email server for Windows, for storing passwords.
13. **ipb2 – IPB2 MD5**: A format used by Invision Power Board (IPB) version 2.x for password storage.
14. **krb4 – Kerberos v4 TGT**: Used for Kerberos version 4 Ticket Granting Tickets.
15. **krb5 – Kerberos v5 TGT**: Used for Kerberos version 5 Ticket Granting Tickets.
16. **lm – LM DES**: The LAN Manager (LM) hash, an old hashing format used by Microsoft for storing passwords.
17. **lotus5 – Lotus5**: Used by Lotus Notes/Domino 5 for password storage.
18. **md4-gen – Generic salted MD4**: A generic format for salted MD4 hashes.
19. **md5 – FreeBSD MD5**: A version of MD5 used in FreeBSD for password hashing.
20. **md5-gen – Generic MD5**: A generic format for MD5 hashes.
21. **mediawiki – MediaWiki MD5s**: Used by MediaWiki for password storage.
22. **mscash – M$ Cache Hash**: Used by Microsoft for caching domain credentials.
23. **mscash2 – M$ Cache Hash 2 (DCC2)**: An updated version of the Microsoft cache hash.
24. **mschapv2 – MSCHAPv2 C/R MD4 DES**: Used in Microsoft's MSCHAPv2 protocol for VPN and WPA2 enterprise.
25. **mskrb5 – MS Kerberos 5 AS-REQ Pre-Auth**: Microsoft's implementation of Kerberos 5 pre-authentication.
26. **mssql05 – MS-SQL05**: Used by Microsoft SQL Server 2005 for password storage.
27. **mssql – MS-SQL**: Used by Microsoft SQL Server for password storage.
28. **mysql-fast – MYSQL_fast**: A fast hash format used by MySQL databases.
29. **mysql – MYSQL**: The standard hash format used by MySQL databases.
30. **mysql-sha1 – MySQL 4.1 double-SHA-1**: Used by MySQL 4.1 and above, applying double SHA-1 hashing.
31. **netlm – LM C/R DES**: Network version of LM hashes used in Windows networks.
32. **netlmv2 – LMv2 C/R MD4 HMAC-MD5**: An updated version of the network LM hash.
33. **netntlm – NTLMv1 C/R MD4 DES [ESS MD5]**: NTLM version 1 challenge/response format.
34. **netntlmv2 – NTLMv2 C/R MD4 HMAC-MD5**: NTLM version 2 challenge/response format.
35. **nethalflm – HalfLM C/R DES**: A format representing half of an LM hash, used in certain Windows network authentication scenarios.
36. **md5ns – Netscreen MD5**: Used by Netscreen devices for password hashing with MD5.
37. **nsldap – Netscape LDAP SHA**: SHA-1 based hash used in Netscape LDAP.
38. **ssha – Netscape LDAP SSHA**: Salted SHA-1 hash used in Netscape LDAP.
39. **nt – NT MD4**: The NT hash, a MD4-based format used in Windows NT, 2000, XP, and later.
40. **openssha – OpenLDAP SSHA**: Salted SHA-1 hash used in OpenLDAP.
41. **oracle11 – Oracle 11g**: Hash format used by Oracle Database 11g.
42. **oracle – Oracle**: Hash format used by Oracle databases.
43. **pdf – PDF**: Used for password hashing in PDF files.
44. **phpass-md5 – PHPass MD5**: Used in PHP applications, notably WordPress, for password hashing.
45. **phps – PHPS MD5**: MD5-based hash used in some PHP applications.
46. **pix-md5 – PIX MD5**: Cisco PIX firewall password hash format.
47. **po – Post.Office MD5**: Used by the Post.Office mail server.
48. **rar – rar**: Used for password protection in RAR archives.
49. **raw-md4 – Raw MD4**: Plain MD4 hash.
50. **raw-md5 – Raw MD5**: Plain MD5 hash.
51. **raw-md5-unicode – Raw MD5 of Unicode plaintext**: MD5 hashing of Unicode plaintext.
52. **raw-sha1 – Raw SHA-1**: Plain SHA-1 hash.
53. **raw-sha224 – Raw SHA-224**: Plain SHA-224 hash.
54. **raw-sha256 – Raw SHA-256**: Plain SHA-256 hash.
55. **raw-sha384 – Raw SHA-384**: Plain SHA-384 hash.
56. **raw-sha512 – Raw SHA-512**: Plain SHA-512 hash.
57. **salted-sha – Salted SHA**: A generic format for salted SHA-1 hashes.
58. **sapb – SAP BCODE**: Used by SAP systems for password hashing.
59. **sapg – SAP CODVN G (PASSCODE)**: Another hash format used by SAP systems.
60. **sha1-gen – Generic salted SHA-1**: A generic format for salted SHA-1 hashes.
61. **skey – S/Key**: One-time password system based on MD4 and MD5.
62. **ssh – ssh**: Used for SSH private keys.
63. **sybasease – sybasease**: Used by Sybase ASE for password storage.
64. **xsha – Mac OS X 10.4+ salted SHA-1**: Used in Mac OS X 10.4 and later for password hashing.
65. **zip – zip**: Used for password-protected ZIP files.