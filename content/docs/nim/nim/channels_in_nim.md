---
title: "Channels in Nim"
description: "Nim Lang description"
icon: "code"
draft: false
---

Nim provides channels as a mechanism for thread communication. Channels allow threads to send and receive messages in a synchronized manner. Here’s a guide on how to use channels in Nim for thread communication.

#### Example with Plain Threads

First, let's look at an example using plain threads. This involves creating and managing threads manually.

```nim
import std/os # for sleep

var
  # create a channel to send/recv strings
  commChan: Channel[string]
  sender: Thread[void]
  recver: Thread[void]

proc sendMsg() =
  sleep(500)
  # send a message in the channel
  commChan.send("Hi")

proc recvMsg() =
  # block on the channel, waiting for output
  let msg: string = commChan.recv()
  echo "Received message: " & msg

# very important: channels must be opened before they can be used
commChan.open()
createThread(sender, sendMsg)
createThread(recver, recvMsg)
joinThreads(sender, recver)
```

In this example:

1. A `Channel[string]` is created to send and receive strings.
2. Two threads (`sender` and `recver`) are created to send and receive messages through the channel.
3. `commChan.open()` is called to open the channel before use.
4. `sendMsg` sends a message after a delay.
5. `recvMsg` waits for and prints the received message.

#### Using `spawn` from `threadpool`

Nim also provides a higher-level abstraction for thread management via the `threadpool` module. This allows for easier creation and management of threads using the `spawn` procedure.

```nim
import threadpool, std/os

var commChan: Channel[string]

proc sendMsg() =
  sleep(500)
  commChan.send("Hi there!")

proc recvMsg() =
  let msg = commChan.recv()
  echo "Received msg: " & msg

commChan.open()
spawn recvMsg()
spawn sendMsg()
sync()
```

In this example:

1. The `spawn` procedure is used to create and run threads.
2. `sync()` is called to wait for all spawned threads to finish.

#### Non-blocking Channel Operations

Channels can also be used in a non-blocking manner. This is useful when you want to attempt to receive messages without blocking the thread if no messages are available.

```nim
while true:
  let tried = commChan.tryRecv()
  if tried.dataAvailable:
    echo tried.msg
```

In this example:

1. `tryRecv()` is used to attempt to receive a message without blocking.
2. If a message is available, it is printed.

#### Limiting Channel Size

You can set a maximum number of items in a channel when opening it. This limits the number of messages that can be queued in the channel.

```nim
# create a channel to transfer ints
var chan: Channel[int]
# allow max of 10 items in channel
chan.open(10)
```

In this example:

1. The channel is opened with a maximum size of 10 items.
2. If the channel is full, sending new messages will block until space becomes available.

To avoid blocking when the channel is full, you can use `trySend`, which returns immediately with a boolean indicating the success of the operation.

```nim
if not chan.trySend(42):
  echo "Failed to send message; channel is full."
```

### Summary

- **Channels**: Used for thread communication in Nim.
- **Opening Channels**: Channels must be opened before use, and can have a specified maximum size.
- **Blocking Operations**: `recv()` blocks until a message is available, and `send()` blocks if the channel is full.
- **Non-blocking Operations**: `tryRecv()` and `trySend()` provide non-blocking alternatives.
- **Thread Management**: Use plain threads with `createThread` or higher-level abstractions with `spawn`.

Channels in Nim offer a powerful way to handle thread communication, allowing both blocking and non-blocking operations, and can be easily integrated with Nim's threading model.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
