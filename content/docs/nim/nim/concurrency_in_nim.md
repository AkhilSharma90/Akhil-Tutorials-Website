---
title: "Concurrency in Nim"
description: "Nim Lang description"
icon: "code"
draft: false
---

Nim provides concurrency using the `async`/`await` syntax, powered by the `asyncdispatch` module. This allows you to write asynchronous functions that can run concurrently on a single thread, making it especially useful for IO-intensive tasks.

#### Key Concepts
- **Concurrency vs. Parallelism**: Concurrency involves managing multiple tasks at the same time, but not necessarily running them simultaneously. Parallelism, on the other hand, means running multiple tasks simultaneously. Nim's async model is focused on concurrency.
- **Async Functions**: Functions that use the `async`/`await` syntax are marked with the `{.async.}` pragma.
- **Await Keyword**: Used to wait for an asynchronous procedure to complete.

#### Example of Concurrency

Here is a basic example demonstrating how to use `async`/`await` in Nim:

```nim
import asyncdispatch

proc ioManager(id: string) {.async.} =
  for i in 1..10:
    # wait for some async process
    await sleepAsync(10)
    echo id & " - run: " & $i

let
  ma = ioManager("a")
  mb = ioManager("b")

waitFor ma and mb
```

**Explanation**:
1. **Async Function**: The `ioManager` function is marked as `{.async.}` and can now use `await`.
2. **Await**: Inside the loop, `await sleepAsync(10)` pauses the execution of `ioManager` for 10 milliseconds without blocking the entire program.
3. **Running Functions**: `waitFor ma and mb` ensures the program waits until both `ma` and `mb` complete.

**Expected Output**:
```
a - run: 1
b - run: 1
a - run: 2
b - run: 2
a - run: 3
b - run: 3
...
```

This output shows the interleaved execution of `ioManager` with different IDs, demonstrating concurrency.

#### Blocking Indefinitely

To block indefinitely waiting for all asynchronous functions to complete, you can use:

```nim
runForever()
```

This is useful for server applications or long-running tasks.

#### Higher Async Modules

The `asyncdispatch` module serves as the foundation for several higher-level modules that provide async functionality for specific purposes:

- **asyncfile**: For asynchronous file operations.
- **asyncnet**: For asynchronous networking operations.
- **asynchttpserver**: For an asynchronous HTTP server.

### Example with `asyncnet`

Here's an example of using `asyncnet` for a simple TCP client:

```nim
import asyncdispatch, asyncnet

proc handleClient(client: AsyncSocket) {.async.} =
  let data = await client.recv(1024)
  echo "Received: ", data
  await client.send("Hello, client!")
  client.close()

proc startServer() {.async.} =
  let server = await newAsyncSocket()
  await server.bindAddr(Port(9000))
  server.listen()

  while true:
    let client = await server.accept()
    handleClient(client)

asyncCheck startServer()
runForever()
```

**Explanation**:
1. **handleClient**: Receives data from the client, sends a response, and then closes the connection.
2. **startServer**: Sets up a server that listens on port 9000 and accepts incoming connections.
3. **asyncCheck**: Ensures `startServer` runs asynchronously.
4. **runForever**: Blocks indefinitely, allowing the server to run continuously.

### Summary

- **Async Functions**: Mark functions with `{.async.}` and use `await` to wait for async operations.
- **waitFor**: Use to wait for the completion of async functions.
- **runForever**: Blocks indefinitely, useful for long-running async tasks.
- **Higher-Level Modules**: Use modules like `asyncfile`, `asyncnet`, and `asynchttpserver` for specific asynchronous operations.

Nim's async/await syntax and related modules provide a powerful and flexible way to handle concurrency, making it easier to write efficient IO-bound programs.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
