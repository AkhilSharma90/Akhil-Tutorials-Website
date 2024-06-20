---
title: "The Domain Name System"
description: "If you are looking to grow in you tech career and understand system design indepth, this guide is for you."
icon: "code"
draft: false
---

# What is DNS?

The Domain Name System (DNS) acts as the Internet's phonebook. Humans access information online using domain names like nytimes.com or espn.com, but web browsers communicate through Internet Protocol (IP) addresses. DNS translates these domain names into IP addresses, allowing browsers to load the correct internet resources. This eliminates the need for users to memorize complex IP addresses, such as 192.168.1.1 for IPv4 or 2400:cb00:2048:1::c629:d7a2 for IPv6.


## How Does DNS Work?

DNS resolution converts a hostname (e.g., www.example.com) into a computer-friendly IP address (e.g., 192.168.1.1). Each device on the Internet has a unique IP address, which functions like a street address, guiding data to the correct destination. When a user wants to load a webpage, a translation must occur between the domain name typed into the browser and the IP address of the server hosting the website.

### Components Involved in DNS Resolution

DNS resolution involves several hardware components that a DNS query must pass through. These components operate behind the scenes without user interaction, ensuring efficient name-to-IP address translation.

#### 1. DNS Recursor

The DNS recursor acts like a librarian tasked with finding a specific book. It receives queries from client machines (such as web browsers) and makes additional requests to satisfy the DNS query.

#### 2. Root Nameserver

The root nameserver is the first step in translating human-readable host names into IP addresses. It acts as an index in a library, pointing to more specific locations.

#### 3. TLD Nameserver

The top-level domain (TLD) nameserver can be compared to a specific rack of books in a library. It hosts the last portion of a hostname (e.g., the "com" in example.com).

#### 4. Authoritative Nameserver

The authoritative nameserver is the final stop in the DNS query chain. It can be thought of as a dictionary on a library rack, translating a specific name into its definition. If the authoritative nameserver has the requested record, it returns the IP address to the DNS recursor, completing the query.

## Difference Between Authoritative DNS Server and Recursive DNS Resolver

### Recursive DNS Resolver

A recursive DNS resolver responds to a recursive request from a client by tracking down the DNS record. It makes a series of requests until it reaches the authoritative DNS nameserver or returns an error if no record is found. Caching helps reduce the number of requests by serving the requested resource record earlier in the DNS lookup process.

### Authoritative DNS Server

An authoritative DNS server holds and is responsible for DNS resource records. It responds with the queried resource record, allowing the web browser to reach the IP address needed to access a website. This server can provide responses from its own data without querying another source.

## Steps in a DNS Lookup

### 8 Steps in a DNS Lookup

1. **User Request**: A user types ‘example.com’ into a web browser.
2. **DNS Recursive Resolver**: The query is received by a DNS recursive resolver.
3. **Root Nameserver**: The resolver queries a DNS root nameserver.
4. **TLD Nameserver**: The root server responds with the address of the TLD DNS server (e.g., .com).
5. **Domain's Nameserver**: The resolver makes a request to the TLD server.
6. **IP Address Retrieval**: The TLD server responds with the IP address of the domain’s nameserver.
7. **Final Query**: The resolver sends a query to the domain’s nameserver.
8. **Response**: The domain’s nameserver returns the IP address to the resolver.

Once the DNS lookup returns the IP address, the browser makes an HTTP request to the IP address, and the server returns the webpage to be rendered in the browser.

## What is a DNS Resolver?

The DNS resolver is the first stop in the DNS lookup process, dealing with the client that made the initial request. It starts the sequence of queries that lead to the translation of a URL into an IP address.

### Types of DNS Queries

1. **Recursive Query**: The DNS client requires the DNS server to respond with the requested resource record or an error message.
2. **Iterative Query**: The DNS client allows the DNS server to return the best answer it can. If there is no match, the server provides a referral to a DNS server authoritative for a lower level of the domain namespace.
3. **Non-Recursive Query**: This occurs when a DNS resolver client queries a DNS server for a record it has access to, either because it is authoritative for the record or it exists in its cache.

### DNS Caching

Caching temporarily stores data to improve performance and reliability. DNS caching involves storing DNS data closer to the requesting client to resolve queries more quickly and reduce bandwidth consumption. Caching can occur at various levels, including:

- **Browser DNS Caching**: Modern web browsers cache DNS records to reduce the number of steps needed to resolve a query.
- **Operating System (OS) Level DNS Caching**: The OS-level DNS resolver checks its cache before sending a DNS query to the Internet service provider (ISP).

### Example: Steps in a DNS Lookup with Caching

1. **Browser Cache Check**: The browser checks its DNS cache for the requested record.
2. **OS Cache Check**: If the record is not found, the OS-level resolver checks its cache.
3. **ISP Resolver Check**: If the OS-level cache does not have the record, the request is sent to the ISP's DNS recursive resolver.


By understanding the DNS process and the roles of different components, users and administrators can ensure efficient and reliable access to internet resources.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).