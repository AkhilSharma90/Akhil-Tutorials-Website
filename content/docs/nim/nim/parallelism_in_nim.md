---
title: "Parallelism in Nim"
description: "Nim Lang description"
icon: "code"
draft: false
---

Parallelism in Nim allows you to execute multiple tasks concurrently, leveraging multiple threads. There are several ways to achieve parallelism, including using threads directly or utilizing higher-level abstractions like threadpool.

#### Enabling Threads

Before using parallelism features, you need to enable threads at compile time:

```bash
nim --threads:on c threads.nim
```

#### Using Threads Directly

```nim
proc sayHi(num: int) {.thread.} =
  echo "Hi from " & $num

var threads: array[10, Thread[int]]

for i in threads.low..threads.high:
  createThread(threads[i], sayHi, i)
joinThreads(threads)
```

**Explanation**:

- `sayHi`: A procedure marked with `{.thread.}` to run in parallel.
- `threads`: An array of thread objects.
- `createThread`: Creates a new thread for each iteration, executing `sayHi`.
- `joinThreads`: Waits for all threads to complete.

#### Using `threadpool`

```nim
import threadpool

proc sayHi(num: int) {.thread.} =
  echo "Hi from " & $num

for i in 0..9:
  spawn sayHi(i)
sync()
```

**Explanation**:

- `spawn`: Creates a new thread to execute `sayHi` for each iteration.
- `sync()`: Waits for all spawned threads to complete.

#### Other Options

- **Experimental `parallel` Statement**: Nim provides an experimental `parallel` statement for parallel execution of code blocks.
- **Returning Data from `spawn` Calls**: It's possible to return data from `spawn` calls using channels or other synchronization mechanisms.

#### Considerations

- **Performance**: Choose the approach that best fits your use case and consider the performance implications of each method.
- **Error Handling**: Implement proper error handling for parallel code to manage exceptions and ensure graceful termination.

Parallelism in Nim offers flexibility and scalability, allowing you to efficiently utilize multiple threads to execute tasks concurrently.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
