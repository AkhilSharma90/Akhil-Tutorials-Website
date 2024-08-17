---
title: "Concurrency in Elixir with OTP"
description: "Learn more about concurrency in Elixir with OTP"
icon: "code"
draft: false
---

Concurrency is one of the core strengths of Elixir, and the language leverages the powerful capabilities of the Erlang VM to build highly concurrent and fault-tolerant systems. In this section, we will explore concurrency in Elixir using OTP (Open Telecom Platform), which is a set of libraries and design principles for building scalable and maintainable applications.

### Introduction

#### Why Concurrency in Elixer?

Elixir, built on the Erlang VM, is designed for building concurrent, distributed, and fault-tolerant applications. The actor model, used by Elixir, provides a robust way to manage concurrency by treating each process as an independent entity that communicates with other processes via message passing.

#### What is OTP?

OTP stands for Open Telecom Platform. It is a collection of middleware, libraries, and tools used to design and implement concurrent, scalable, and fault-tolerant systems. OTP provides several abstractions like GenServer, Supervisor, and Application, which simplify building and maintaining complex applications.

### Processes in Elixir

##### Creating Processes

In Elixir, processes are lightweight and run in isolation. They are created using the spawn function:

```elixir
defmodule SimpleProcess do
  def greet do
    IO.puts("Hello from a process!")
  end
end

pid = spawn(SimpleProcess, :greet, [])
```

Here, spawn/3 creates a new process that executes the greet function

##### Sending and Receiving Messages

Processes communicate via message passing. You can send a message to a process using the send function and receive messages using the receive block:

```elixir
defmodule Messenger do
  def loop do
    receive do
      {:msg, sender, message} ->
        IO.puts("Received message: #{message}")
        send(sender, {:ok, self()})
        loop()
    end
  end
end

pid = spawn(Messenger, :loop, [])
send(pid, {:msg, self(), "Hello"})
receive do
  {:ok, _pid} -> IO.puts("Message received successfully!")
end
```

### GenServer

GenServer is a generic server implementation that abstracts the common patterns of working with processes. It simplifies the implementation of servers in Elixir by providing a standard way to define and handle state, callbacks, and message passing.

##### Creating a GenServer

To create a GenServer, you need to define a module that uses GenServer and implement the required callbacks:

```elixir
defmodule MyGenServer do
  use GenServer

  # Client API
  def start_link(initial_state) do
    GenServer.start_link(__MODULE__, initial_state, name: __MODULE__)
  end

  def get_state do
    GenServer.call(__MODULE__, :get_state)
  end

  def set_state(new_state) do
    GenServer.cast(__MODULE__, {:set_state, new_state})
  end

  # Server Callbacks
  @impl true
  def init(initial_state) do
    {:ok, initial_state}
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_cast({:set_state, new_state}, _state) do
    {:noreply, new_state}
  end
end
```

##### Using the GenServer

You can start and interact with the GenServer using the client API functions:

```elixir
{:ok, _pid} = MyGenServer.start_link(%{count: 0})
IO.inspect(MyGenServer.get_state())  # Output: %{count: 0}
MyGenServer.set_state(%{count: 42})
IO.inspect(MyGenServer.get_state())  # Output: %{count: 42}
```

### Supervisors

Supervisors are a core part of OTP that provide fault tolerance by monitoring processes and restarting them if they fail. Supervisors are designed to manage process lifecycles, making it easier to build resilient systems.

##### Creating a Supervisor

To create a supervisor, define a module that uses Supervisor and specify a supervision strategy:

```elixir
defmodule MySupervisor do
  use Supervisor

  def start_link(_) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      {MyGenServer, %{count: 0}}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

##### Starting the Supervisor

You can start the supervisor and it will automatically start its child processes:

```elixir
{:ok, _pid} = MySupervisor.start_link(:ok)
IO.inspect(MyGenServer.get_state())  # Output: %{count: 0}
MyGenServer.set_state(%{count: 42})
IO.inspect(MyGenServer.get_state())  # Output: %{count: 42}
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
