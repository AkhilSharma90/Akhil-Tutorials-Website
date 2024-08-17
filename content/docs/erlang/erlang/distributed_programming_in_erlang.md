---
title: "Distributed Programming"
description: ""
icon: "code"
draft: false
---

Distributed programming involves writing programs designed to run on networks of computers, coordinating their activities through message passing. Erlang is well-suited for distributed programming due to its built-in support for creating and managing distributed systems.

## Why Write Distributed Applications?

There are several reasons to write distributed applications:

### Performance

By running different parts of a program in parallel on different machines, we can significantly improve performance.

### Reliability

We can achieve fault tolerance by distributing the system across multiple machines. If one machine fails, the system can continue running on another machine.

### Scalability

As applications grow, they may outgrow the capabilities of a single machine. By adding more machines, we can increase the capacity of the system without major changes to the application architecture.

## Central Concept: The Node

In distributed Erlang, the central concept is the node. A node is a self-contained Erlang runtime system, complete with its own virtual machine, address space, and set of processes.

## Methods for Distributed Programming

Here are some essential methods for distributed programming in Erlang:

1. **spawn**
   - Used to create a new process and initialize it.
2. **node**
   - Used to determine the value of the node on which the process is running.
3. **spawn on Node**
   - Used to create a new process on a specific node.
4. **is_alive**
   - Returns `true` if the local node is alive and can be part of a distributed system.
5. **spawnlink**
   - Used to create a new process link on a node.

## Example: Setting Up Distributed Erlang Nodes

Let's go through an example of setting up a distributed Erlang system.

### 1. Starting Nodes

First, we need to start Erlang nodes. Open two terminal windows and start two Erlang shells with different node names:

**Node 1:**

```shell
$ erl -sname node1@localhost
```

**Node 2:**

```shell
$ erl -sname node2@localhost
```

### 2. Making Nodes Aware of Each Other

Make the nodes aware of each other using the `net_adm:ping/1` function:

**Node 1 Shell:**

```erlang
(node1@localhost)1> net_adm:ping(node2@localhost).
pong
```

**Node 2 Shell:**

```erlang
(node2@localhost)1> net_adm:ping(node1@localhost).
pong
```

### 3. Creating a Process on a Remote Node

Create a module `distributed_example.erl`:

```erlang
-module(distributed_example).
-export([start/0, remote_hello/0]).

remote_hello() ->
    io:format("Hello from ~p~n", [node()]).

start() ->
    spawn(node2@localhost, distributed_example, remote_hello, []).
```

### 4. Running the Example

Compile and run the example from Node 1:

```erlang
(node1@localhost)2> c(distributed_example).
{ok,distributed_example}
(node1@localhost)3> distributed_example:start().
<0.55.0>
```

Check the output on Node 2 to see the message from `remote_hello/0`.

### 5. Checking Node Status

You can check if a node is alive using `node/0` and `is_alive/0`:

**Node 1 Shell:**

```erlang
(node1@localhost)4> node().
node1@localhost
(node1@localhost)5> node2@localhost.
true
```

## Conclusion

Erlang's built-in support for distributed programming makes it a powerful tool for creating scalable, reliable, and high-performance applications. By understanding the key methods and concepts, you can leverage Erlang to build robust distributed systems.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
