---
title: "Web Programming in Erlang"
description: ""
icon: "code"
draft: false
---

In Erlang, the `inets` library provides the functionality to build web servers, known as `httpd`, to handle HTTP requests. This tutorial will guide you through setting up a basic web server in Erlang and implementing a "Hello, World!" web application.

## Features of Erlang HTTP Server

The Erlang HTTP server (`httpd`) supports several features:

- Secure Sockets Layer (SSL)
- Erlang Scripting Interface (ESI)
- Common Gateway Interface (CGI)
- User Authentication (using Mnesia, Dets, or plain text database)
- Common Logfile Format (with or without `disk_log` support)
- URL Aliasing
- Action Mappings
- Directory Listings

## Setting Up the Web Server

### Starting the Web Library

First, start the `inets` library:

```erlang
inets:start().
```

### Implementing the Web Server

Create a module to implement the web server process.

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    inets:start(),
    {ok, Pid} = inets:start(httpd, [
        {port, 8081},
        {server_name, "httpd_test"},
        {server_root, "D://tmp"},
        {document_root, "D://tmp/htdocs"},
        {bind_address, "localhost"}
    ]),
    io:fwrite("~p", [Pid]).
```

### Notes

- The `port` parameter specifies the port number on which the `httpd` service will run. Ensure this port is not used by any other service.
- The `server_root` and `document_root` parameters are mandatory and define the root directories for the server and documents, respectively.

### Output

The output will show the process identifier:

```plaintext
{ok,<0.42.0>}
```

## Creating a "Hello, World!" Web Server

### Step 1: Implement the Server Code

Create a module with the following code:

```erlang
-module(helloworld).
-export([start/0, service/3]).

start() ->
    inets:start(httpd, [
        {modules, [
            mod_alias,
            mod_auth,
            mod_esi,
            mod_actions,
            mod_cgi,
            mod_dir,
            mod_get,
            mod_head,
            mod_log,
            mod_disk_log
        ]},
        {port, 8081},
        {server_name, "helloworld"},
        {server_root, "D://tmp"},
        {document_root, "D://tmp/htdocs"},
        {erl_script_alias, {"/erl", [helloworld]}},
        {error_log, "error.log"},
        {security_log, "security.log"},
        {transfer_log, "transfer.log"},
        {mime_types, [
            {"html", "text/html"},
            {"css", "text/css"},
            {"js", "application/x-javascript"}
        ]}
    ]).

service(SessionID, _Env, _Input) ->
    mod_esi:deliver(SessionID, [
        "Content-Type: text/html\r\n\r\n",
        "<html><body>Hello, World!</body></html>"
    ]).
```

### Step 2: Compile and Run the Code

Compile the module and start the server:

```erlang
1> c(helloworld).
{ok,helloworld}
2> inets:start().
ok
3> helloworld:start().
{ok,<0.50.0>}
```

### Step 3: Access the Web Page

Open a web browser and navigate to the URL:

```plaintext
http://localhost:8081/erl/helloworld:service
```

You should see the "Hello, World!" message displayed.

## Conclusion

In this tutorial, we explored how to set up a basic web server in Erlang using the `inets` library. By following these steps, you can create a simple web application and extend it with more features as needed.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
