---
title: "Multi-Thread Programming with Julia"
description: "Julia, a high-level, high-performance programming language, is designed for technical computing"
icon: "code"
draft: false
---

#### Starting Julia with Multiple Threads

By default, Julia starts with a single thread of execution. This can be verified with the command `Threads.nthreads()`:

```julia
julia> Threads.nthreads()
1
```

The number of execution threads is controlled either by using the `-t/--threads` command line argument or by setting the `JULIA_NUM_THREADS` environment variable. If both are specified, `-t/--threads` takes precedence.

You can specify the number of threads either as an integer (`--threads=4`) or as `auto` (`--threads=auto`), where `auto` tries to infer a useful default number of threads (see Command-line Options for more details).

**Julia 1.5**: The `-t/--threads` command line argument requires at least Julia 1.5. In older versions, you must use the environment variable.

**Julia 1.7**: Using `auto` as the value of the environment variable `JULIA_NUM_THREADS` requires at least Julia 1.7. In older versions, this value is ignored.

To start Julia with 4 threads:

```bash
$ julia --threads 4
```

Verify there are 4 threads available:

```julia
julia> Threads.nthreads()
4
```

Currently, you are on the master thread. To check:

```julia
julia> Threads.threadid()
1
```

If you prefer to use the environment variable, set it as follows:

- **Bash (Linux/macOS)**: `export JULIA_NUM_THREADS=4`
- **C shell on Linux/macOS, CMD on Windows**: `set JULIA_NUM_THREADS=4`
- **Powershell on Windows**: `$env:JULIA_NUM_THREADS=4`

This must be done before starting Julia.

The number of threads specified with `-t/--threads` is propagated to worker processes spawned using the `-p/--procs` or `--machine-file` command line options. For example, `julia -p2 -t2` spawns 1 main process with 2 worker processes, all having 2 threads enabled. For more control over worker threads, use `addprocs` and pass `-t/--threads` as `exeflags`.

#### Multiple GC Threads

The Garbage Collector (GC) can use multiple threads. The number used is either half the number of compute worker threads or is configured by the `--gcthreads` command line argument or by the `JULIA_NUM_GC_THREADS` environment variable.

**Julia 1.10**: The `--gcthreads` command line argument requires at least Julia 1.10.

#### Threadpools

When a program's threads are busy with many tasks, tasks may experience delays, affecting the program's responsiveness. To address this, you can specify a task as interactive when you use `Threads.@spawn`:

```julia
using Base.Threads
@spawn :interactive f()
```

Interactive tasks should avoid high-latency operations and, if long-running, should yield frequently.

Julia can be started with one or more threads reserved for interactive tasks:

```bash
$ julia --threads 3,1
```

Similarly, using the environment variable:

```bash
export JULIA_NUM_THREADS=3,1
```

This starts Julia with 3 threads in the `:default` threadpool and 1 thread in the `:interactive` threadpool:

```julia
julia> using Base.Threads

julia> nthreadpools()
2

julia> threadpool() # the main thread is in the interactive thread pool
:interactive

julia> nthreads(:default)
3

julia> nthreads(:interactive)
1

julia> nthreads()
3
```

The zero-argument version of `nthreads` returns the number of threads in the default pool. Depending on whether Julia has been started with interactive threads, the main thread is either in the default or interactive thread pool.

Either or both numbers can be replaced with the word `auto`, which causes Julia to choose a reasonable default.

#### Communication and Synchronization

Although Julia's threads can communicate through shared memory, writing correct and data-race-free multi-threaded code is challenging. Julia's Channels are thread-safe and may be used to communicate safely.

**Data-Race Freedom**: It is your responsibility to ensure your program is data-race free. Using locks around any access to data shared between multiple threads is essential. For example:

```julia
julia> lock(lk) do
           use(a)
       end

julia> begin
           lock(lk)
           try
               use(a)
           finally
               unlock(lk)
           end
       end
```

Where `lk` is a lock (e.g., `ReentrantLock()`) and `a` is data.

Julia is not memory safe in the presence of a data race. Be careful about reading data if another thread might write to it! Always use the lock pattern when changing data accessed by other threads.

#### The `@threads` Macro

Let's use native threads in a simple example. Create an array of zeros:

```julia
julia> a = zeros(10)
10-element Vector{Float64}:
 0.0
 0.0
 0.0
 0.0
 0.0
 0.0
 0.0
 0.0
 0.0
 0.0
```

Operate on this array simultaneously using 4 threads, with each thread writing its thread ID into each location. The `Threads.@threads` macro indicates that the loop is a multi-threaded region:

```julia
julia> Threads.@threads for i = 1:10
           a[i] = Threads.threadid()
       end
```

The iteration space is split among the threads, each writing its thread ID to its assigned locations:

```julia
julia> a
10-element Vector{Float64}:
 1.0
 1.0
 1.0
 2.0
 2.0
 2.0
 3.0
 3.0
 4.0
 4.0
```

Note that `Threads.@threads` does not have an optional reduction parameter like `@distributed`.

#### Using `@threads` Without Data Races

Consider a naive sum function:

```julia
julia> function sum_single(a)
           s = 0
           for i in a
               s += i
           end
           s
       end
sum_single (generic function with 1 method)

julia> sum_single(1:1_000_000)
500000500000
```

Simply adding `@threads` exposes a data race with multiple threads reading and writing `s` at the same time:

```julia
julia> function sum_multi_bad(a)
           s = 0
           Threads.@threads for i in a
               s += i
           end
           s
       end
sum_multi_bad (generic function with 1 method)

julia> sum_multi_bad(1:1_000_000)
70140554652
```

The result is incorrect and changes with each evaluation.

To fix this, use buffers specific to each task to segment the sum into chunks that are race-free. Reuse `sum_single`, which has its own internal buffer `s`, and split vector `a` into `nthreads()` chunks for parallel work via `nthreads() @spawn`-ed tasks:

```julia
julia> function sum_multi_good(a)
           chunks = Iterators.partition(a, length(a) ÷ Threads.nthreads())
           tasks = map(chunks) do chunk
               Threads.@spawn sum_single(chunk)
           end
           chunk_sums = fetch.(tasks)
           return sum_single(chunk_sums)
       end
sum_multi_good (generic function with 1 method)

julia> sum_multi_good(1:1_000_000)
500000500000
```

Buffers should not be managed based on `threadid()`, as tasks can yield and use the same buffer on a given thread, introducing data race risks. Task migration means tasks may change threads at yield points.

Alternatively, use atomic operations on variables shared across tasks/threads, which may be more performant depending on the operation's characteristics.

#### Atomic Operations

Julia supports accessing and modifying values atomically, avoiding race conditions. A value (which must be a primitive type) can be wrapped as `Threads.Atomic` to indicate thread-safe access. Here’s an example:

```julia
julia> i = Threads.Atomic{Int}(0);

julia> ids = zeros(4);

julia> old_is = zeros(4);

julia> Threads.@threads for id in 1:4
           old_is[id] = Threads.atomic_add!(i, id)
           ids[id] = id
       end

julia> old_is
4-element Vector{Float64}:
 0.0
 1.0
 7.0
 3.0

julia> i[]
 10

julia> ids
4-element Vector{Float64}:
 1.0
 2.0
 3.0
 4.0
```

Without the atomic tag, a race condition might produce incorrect results:

```julia
julia> using Base.Threads

julia> Threads.nthreads()
4

julia> acc = Ref(0)
Base.RefValue{Int64}(0)

julia> @threads for i

 = 1:1000
           acc[] += 1
       end

julia> acc
Base.RefValue{Int64}(955)
```

Use `Threads.atomic_add!` to avoid this:

```julia
julia> acc = Atomic{Int64}(0)
Base.Threads.Atomic{Int64}(0)

julia> @threads for i = 1:1000
           Threads.atomic_add!(acc, 1)
       end

julia> acc
Base.Threads.Atomic{Int64}(1000)
```

This ensures `acc` is properly incremented to 1000.

#### Tasks (aka Coroutines)

Tasks support concurrent operations, useful when tasks depend on IO and avoid simultaneous processing.

Use `@async` to create tasks:

```julia
julia> t = @async 1 + 1
Task (runnable) @0x00007f9a104ca550

julia> fetch(t)
2
```

```julia
julia> t = @async begin
           sleep(0.5)
           1 + 1
       end
Task (runnable) @0x00007f9a14d0dcd0

julia> fetch(t)
2
```

**`@async`**

This macro is syntactic sugar for a `Task` constructor followed by `schedule`:

```julia
julia> t = Task(() -> begin
           sleep(0.5)
           1 + 1
       end)
Task (runnable) @0x00007f9a14d66ed0

julia> schedule(t)
Task (runnable) @0x00007f9a14d66ed0

julia> fetch(t)
2
```

Use `schedule` to return the task for chaining with other functions:

```julia
julia> fetch(schedule(Task(() -> (sleep(0.5); 1 + 1))))
2
```

`@async` schedules and returns the task:

```julia
julia> fetch(@async (sleep(0.5); 1 + 1))
2
```

**Channels**

Channels communicate data between tasks. The simplest form:

```julia
julia> channel = Channel{Int}(10);

julia> produce(channel) = for i in 1:20
           put!(channel, i)
       end;

julia> c = @async produce(channel);

julia> while isopen(channel)
           println(take!(channel))
       end
```

**Channel Creation**

`Channel{T}(sz)`: Creates a channel of type `T` and size `sz`.

Without specifying a buffer size, an unbuffered channel is created:

```julia
julia> c = Channel{Int}(0)
Channel{Int64}(sz_max:0,sz_curr:0)

julia> @async put!(c, 1)

julia> take!(c)
1
```

Task blocking occurs when performing actions on unbuffered channels without available counterparts. The `Channel` constructor can take a function to execute:

```julia
julia> c = Channel(32) do c
           for n = 1:32
               put!(c, n)
           end
       end;

julia> for x in c
           println(x)
       end
```

**Remote Channels**

Remote channels enable remote storage and retrieval of values, utilizing remote workers for parallel computations.

To use a remote channel, start Julia with multiple worker processes using `-p` or `--machine-file`:

```bash
julia -p n
```

In the REPL, manage remote channels:

```julia
julia> addprocs(2)
2-element Vector{Int64}:
 2
 3

julia> @everywhere foo() = 1

julia> @spawnat :any foo()
Future(2, 1, 4, nothing)

julia> fetch(@spawnat :any foo())
1
```

Remote channels provide remote communication through an API similar to local channels. They can be created on specific or remote workers:

```julia
julia> r = RemoteChannel(()->Channel{Int}(10), 2)
RemoteChannel{Channel{Int64}}(2, 1, 7)

julia> @async begin
           for i in 1:10
               put!(r, i)
           end
       end;

julia> fetch(@spawnat 2 take!(r))
1
```

In this example, the remote channel `r` is created on worker 2 with a buffer size of 10, and values are asynchronously placed and fetched.

Explore these threading and task capabilities to harness the full power of Julia's concurrency model for efficient, parallelized applications.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
