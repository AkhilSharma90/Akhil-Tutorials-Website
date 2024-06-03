---
title: "Build A Chat App using Elixir"
description: "In this section, we will build a simple chat app that takes advantage of phoenix’s easy uses of web sockets and evented programming"
icon: "code"
draft: false
---

Through this tutorial, we are going to gain a better understanding of web sockets and learn how to take advantage of Elixir's Asynchronous programming model.

## Creating a new project
We will create a new project named `chat_app`
```bash
mix phx.new chat_app
# next
cd chat_app
mix ecto.create
```

Once these are done, you should see something like

