---
description: 
linkTitle: Burp Suite
title: Burp Suite Cheat sheet
weight: 10
---

### Tool Specific Hotkeys

- `Ctrl-F`: Forward Request (Proxy)
- `Ctrl-T`: Toggle Proxy Intercept On/Off
- `Ctrl-Space`: Send Request (Repeater)
- `Double-click TAB`: Rename a tab

### Navigational Hotkeys

- `Ctrl-Shift-T`: Target Tab
- `Ctrl-Shift-P`: Proxy Tab
- `Ctrl-Shift-R`: Repeater Tab
- `Ctrl-Shift-I`: Intruder Tab
- `Ctrl-Shift-O`: Project Options Tab
- `Ctrl-Shift-D`: Dashboard Tab
- `Ctrl-Equal`: Next tab
- `Ctrl-Minus`: Previous tab

### Editor Encoding / Decoding Hotkeys

- `Ctrl-B`: Base64 selection
- `Ctrl-Shift-B`: Base64 decode selection
- `Ctrl-H`: Replace with HTML Entities (key characters only)
- `Ctrl-Shift-H`: Replace HTML entities with characters
- `Ctrl-U`: URL encode selection (key characters only)
- `Ctrl-Shift-U`: URL decode selection

### Global Hotkeys

- `Ctrl-I`: Send to Intruder
- `Ctrl-R`: Send to Repeater
- `Ctrl-S`: Search (places cursor in search field)
- `Ctrl-.`: Go to next selection
- `Ctrl-,`: Go to previous selection
- `Ctrl-A`: Select all
- `Ctrl-Z`: Undo
- `Ctrl-Y`: Redo

### Editors Hotkeys

- `Ctrl-Delete`: Delete Word
- `Ctrl-D`: Delete Line
- `Ctrl-Backspace`: Delete Word Backwards
- `Ctrl-Home`: Go to beginning of the document
- `Ctrl-End`: Go to end of the document
- `Ctrl-Left/Right`: Navigate words
- `Ctrl-Shift`: Select data on its way

### Hunting for Vulnerabilities

- `Param Miner`: Identifies unlinked parameters.
- `Backslash Powered Scanner`: Alerts on data transformations.
- `Software Vulnerability Scanner`: Checks software versions against known vulnerabilities.

### Authorization and Authentication

- `SAML-Raider`: Inspect and modify SAML messages.
- `JSON Web Tokens`: Decode and manipulate JWTs on the fly.
- `Autorize`: Check access control for different roles or unauthenticated users.

### More Vulnerability Hunting Tools

- `HTTP Request Smuggler`: Launch HTTP Request Smuggling attacks.
- `Active Scan++`: Additional vulnerability scanning capabilities.
- `Retire.js`: Identify outdated JavaScript libraries with associated CVEs.

### Utilities

- `Logger++`: Log and monitor attacks; sort by status code.
- `Turbo Intruder`: High-speed, customizable HTTP request sending.
- `Taborator`: Ease Burp Collaborator usage for call-back vulnerabilities.

### REST API

- Enable in user options, default access at `http://127.0.0.1:1337/`.
- Interact via web application, not just CLI.
- Use cURL commands for interaction with Burp's features in headless mode.

### API Examples

- List defined issues: `curl -X GET 'http://127.0.0.1:1337/v0.1/knowledge_base/issue_definitions'`
- Scan a URL: `curl -X POST 'http://127.0.0.1:1337/v0.1/scan' -d '{"urls":["http://target1.com","http://target2.com"]}'`
- Check scan status: `curl -X GET 'http://127.0.0.1:1337/v0.1/scan/<task_id>'`