---
title: "Asynchronous Programming with Julia"
description: "Julia, a high-level, high-performance programming language, is designed for technical computing"
icon: "code"
draft: false
---

When a program needs to interact with external systems, such as communicating with another machine over the internet, the sequence of operations may become unpredictable. For instance, if your program needs to download a file, you would initiate the download, perform other tasks while waiting for the download to complete, and then resume the code that requires the downloaded file once it's available. This scenario is an example of asynchronous programming, also known as concurrent programming, as multiple operations appear to happen simultaneously.

To handle these situations, Julia provides Tasks (also referred to as symmetric coroutines, lightweight threads, cooperative multitasking, or one-shot continuations). Designating a piece of work (typically, executing a particular function) as a Task allows it to be interrupted and switched to another Task. The original Task can later be resumed from where it was paused. While this may resemble a function call, there are two crucial differences. First, task switching doesn't use up stack space, allowing numerous task switches without affecting the call stack. Second, tasks can switch in any order, unlike function calls, where the called function must complete before control returns to the caller.

### Basic Task Operations
A Task can be viewed as a handle to a unit of computational work. It follows a create-start-run-finish lifecycle. Tasks are created by calling the Task constructor on a zero-argument function or using the @task macro:

```julia
julia> t = @task begin; sleep(5); println("done"); end
Task (runnable) @0x00007f13a40c0eb0
```

Here, `@task x` is equivalent to `Task(()->x)`.

This task waits for five seconds and then prints "done". However, it hasn't started running yet. We can start it by calling `schedule`:

```julia
julia> schedule(t);
```

If you try this in the REPL, you'll notice that `schedule` returns immediately because it only adds `t` to an internal queue of tasks to be run. The REPL then prints the next prompt and waits for more input. This idle time allows other tasks to run, so `t` starts. It calls `sleep`, sets a timer, and pauses execution. Other scheduled tasks might run during this period. After five seconds, the timer triggers and `t` resumes, printing "done" and then finishing.

The `wait` function blocks the calling task until another task finishes. For example, if you type:

```julia
julia> schedule(t); wait(t)
```

instead of only calling `schedule`, you will observe a five-second pause before the next input prompt appears, as the REPL waits for `t` to finish before continuing.

It is often useful to create and schedule a task immediately, so the `@async` macro is provided for this purpose – `@async x` is equivalent to `schedule(@task x)`.

### Communicating with Channels
In some cases, different tasks are not naturally related by function calls; there isn't an obvious "caller" or "callee". An example is the producer-consumer problem, where one procedure generates values and another consumes them. The consumer cannot simply call a producer function to get a value because the producer may still be generating values and might not be ready to return. With tasks, both the producer and consumer can run as long as needed, passing values back and forth as necessary.

Julia offers a Channel mechanism to address this issue. A Channel is a waitable first-in-first-out queue that multiple tasks can read from and write to.

Let's define a producer task that produces values using the `put!` call. To consume values, we schedule the producer to run in a new task. A special Channel constructor that accepts a one-argument function can be used to run a task bound to a channel. We can then repeatedly `take!` values from the channel object.

```julia
julia> function producer(c::Channel)
           put!(c, "start")
           for n=1:4
               put!(c, 2n)
           end
           put!(c, "stop")
       end;

julia> chnl = Channel(producer);

julia> take!(chnl)
"start"

julia> take!(chnl)
2

julia> take!(chnl)
4

julia> take!(chnl)
6

julia> take!(chnl)
8

julia> take!(chnl)
"stop"
```

Note that we did not have to explicitly close the channel in the producer. This is because the act of binding a Channel to a Task associates the open lifetime of a channel with that of the bound task. The channel object is closed automatically when the task terminates. Multiple channels can be bound to a task, and vice-versa.

While the Task constructor expects a 0-argument function, the Channel method that creates a task-bound channel expects a function that accepts a single argument of type Channel. A common pattern is for the producer to be parameterized, in which case a partial function application is needed to create a 0 or 1 argument anonymous function.

For Task objects this can be done either directly or by use of a convenience macro:

```julia
function mytask(myarg)
    ...
end

taskHdl = Task(() -> mytask(7))
# or, equivalently
taskHdl = @task mytask(7)
```

To orchestrate more advanced work distribution patterns, bind and schedule can be used in conjunction with Task and Channel constructors to explicitly link a set of channels with a set of producer/consumer tasks.

### More on Channels
A channel can be visualized as a pipe, i.e., it has a write end and a read end :
- Multiple writers in different tasks can write to the same ch
annel concurrently via put! calls.
- Multiple readers in different tasks can read data concurrently vi
a take! calls.
- As an example:

```julia
# Given Channels c1 and c2,
c1 = Channel(32)
c2 = Channel(32)

# and a function `foo` which reads items from c1, processes the item read
# and writes a result to c2,
function foo()
    while true
        data = take!(c1)
        [...]               # process data
        put!(c2, result)    # write out result
    end
end

# we can schedule `n` instances of `foo` to be active concurrently.
for _ in 1:n
    errormonitor(@async foo())
end
```

- Channels are created via the Channel{T}(sz) constructor. The channel will only hold objects of type T. If the type is not specified, the channel can hold objects of any type. sz refers to the maximum number of elements that can be held in the channel at any time. For example, Channel(32) creates a channel that can hold a maximum of 32 objects of any type. A Channel{MyType}(64) can hold up to 64 objects of MyType at any time.
- If a Channel is empty, readers (on a take! call) will block until data is available.
- If a Channel is full, writers (on a put! call) will block until space becomes available.
- isready tests for the presence of any object in the channel, while wait waits for an object to become available.
- A Channel is in an open state initially. This means that it can be read from and written to freely via take! and put! calls. close closes a Channel. On a closed Channel, put! will fail. For example:
```julia
julia> c = Channel(2);

julia> put!(c, 1) # `put!` on an open channel succeeds
1

julia> close(c);

julia> put!(c, 2) # `put!` on a closed channel throws an exception.
ERROR: InvalidStateException: Channel is closed.
Stacktrace:
[...]
```
take! and fetch (which retrieves but does not remove the value) on a closed channel successfully return any existing values until it is emptied. Continuing the above example:

```julia
julia> fetch(c) # Any number of `fetch` calls succeed.
1

julia> fetch(c)
1

julia> take!(c) # The first `take!` removes the value.
1

julia> take!(c) # No more data available on a closed channel.
ERROR: InvalidStateException: Channel is closed.
Stacktrace:
[...]
```

Consider a simple example using channels for inter-task communication. We start 4 tasks to process data from a single jobs channel. Jobs, identified by an id (job_id), are written to the channel. Each task in this simulation reads a job_id, waits for a random amount of time and writes back a tuple of job_id and the simulated time to the results channel. Finally all the results are printed out.

```julia
julia> const jobs = Channel{Int}(32);

julia> const results = Channel{Tuple}(32);

julia> function do_work()
           for job_id in jobs
               exec_time = rand()
               sleep(exec_time)                # simulates elapsed time doing actual work
                                               # typically performed externally.
               put!(results, (job_id, exec_time))
           end
       end;

julia> function make_jobs(n)
           for i in 1:n
               put!(jobs, i)
           end
       end;

julia> n = 12;

julia> errormonitor(@async make_jobs(n)); # feed the jobs channel with "n" jobs

julia> for i in 1:4 # start 4 tasks to process requests in parallel
           errormonitor(@async do_work())
       end

julia> @elapsed while n > 0 # print out results
           job_id, exec_time = take!(results)
           println("$job_id finished in $(round(exec_time; digits=2)) seconds")
           global n = n - 1
       end
4 finished in 0.22 seconds
3 finished in 0.45 seconds
1 finished in 0.5 seconds
7 finished in 0.14 seconds
2 finished in 0.78 seconds
5 finished in 0.9 seconds
9 finished in 0.36 seconds
6 finished in 0.87 seconds
8 finished in 0.79 seconds
10 finished in 0.64 seconds
12 finished in 0.5 seconds
11 finished in 0.97 seconds
0.029772311
```

### Tasks and Events
Most task switches occur as a result of waiting for events like I/O requests and are managed by a scheduler included in Julia Base. The scheduler maintains a queue of runnable tasks and runs an event loop that restarts tasks based on external events such as message arrivals.

The primary function for waiting for an event is `wait`. Several objects implement `wait`; for instance, given a `Process` object, `wait` will wait for it to exit. Often, `wait` is implicit; for example, a call to `read` might internally wait for data to become available.

In all these cases, `wait` operates on a `Condition` object, responsible for queueing and restarting tasks. When a task calls `wait` on a `Condition`, the task is marked as non-runnable, added to the condition's queue, and switches to the scheduler. The scheduler then picks another task to run or blocks waiting for external events. When the event occurs, an event handler calls `notify` on the condition, making the tasks waiting on that condition runnable again.

A task explicitly created by calling `Task` is initially unknown to the scheduler. This allows for manual task management using `yieldto` if desired. However, when such a task waits for an event, it will still be automatically restarted when the event occurs, as expected.