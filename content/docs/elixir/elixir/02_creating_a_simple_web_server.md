---
title: "Create A Simple Web Server using Elixir"
description: "We are going to be creating a simple web server using elixir and cowboy"
icon: "code"
draft: false
---

In this lesson, we will build a simple HTTP server from scratch using the `PlugCowboy` Elixir library. Cowboy is a simple HTTP server for Erlang and Plug will provide us with a connection adapter for the web server. 

### Getting started
Assuming you already have Elixir installed, we will start a simple project by running the command:
```elixir
mix new server --sup
cd server
```
Note that `server` is the name of your project, and you can name it however you want. We also added `--sup` because our app needs a supervision tree because we will use a Supervision to start up and run our `Cowboy` server.

### Adding the Cowboy dependancy

Adding dependencies is way simpler than you thought. To use `Plug` as an adapter interface for `Cowboy` webserver, we need to install `PlugCowboy` package. Open your `mix.exs` file and add the following code

```elixir
def deps do
  [
    {:plug_cowboy, "~> 2.0"},
  ]
end
```

Now, your complete `mix.exs` file should look like this :
```elixir
defmodule Server.MixProject do
  use Mix.Project

  def project do
    [
      app: :server,
      version: "0.1.0",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Server.Application, []}
    ]
  end

  defp deps do
    [
      {:plug_cowboy, "~> 2.0"}
    ]
  end
end
```

Now you can simply install the dependencie by runnig the following command in your terminal:
```elixir
mix deps.get
```

### Building our router

To handle requests and send responses, we have to make a router. You can use the example below to build it. Simply create a `router.ex` file in your `lib` folder and have this code in it:
```elixir
defmodule Server.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/" do
    send_resp(conn, 200, "Hello World")
  end

  match _ do
    send_resp(conn, 404, "Not found")
  end
end
```

Here, we are making GET request to "/" and it will send back "Hello World". If the URL you are visiting is not defined in the routes, it will send back "Not Found". You can add as many routes as you want here. You can of course do more than just receiving GET requests. For our demonstation, we will focus only on this one.

### Configuring our application module

We need to tell our application to start up and supervise the Cowboy web server when the app starts up.

We’ll do so with the `Plug.Cowboy` function. This function expects three options:

- `:scheme` - HTTP or HTTPS as an atom (:http, :https)
- `:plug` - The plug module to be used as the interface for the web server. You can specify a module name, like MyPlug, or a tuple of the module name and options {MyPlug, plug_opts}, where plug_opts gets passed to your plug modules init/1 function.
- `:options` - The server options. Should include the port number on which you want your server listening for requests.

Open your `application.ex` in the `lib/server/application.ex` and replace the code with this:

```elixir
defmodule Server.Application do
  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    children = [
      {Plug.Cowboy, scheme: :http, plug: Server.Router, options: [port: 4000]}
    ]

    opts = [strategy: :one_for_one, name: Server.Supervisor]
    Logger.info("starting the application...")
    Supervisor.start_link(children, opts)
  end
end
```

### Running our server
No we are finished setting up everything an we can finally run our server. We can use the following command:
```elixir
mix run --no-halt
```
Using your browser or postman, you can enter the request `localhost:4000/` and you should see the `Hello World` message.

You have just learnt how to set up and configure a server for your elixir project. You can add more route definitions and try it out unti you get it. Remember, repitition is key.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
