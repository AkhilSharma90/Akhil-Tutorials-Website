---
title: "Concurrency in Zig"
description: "Zig is a general-purpose programming language and toolchain for maintaining robust, optimal, and reusable software."
icon: "code"
draft: false
---

Concurrency is a fundamental aspect of modern software development, enabling programs to execute multiple tasks concurrently for improved performance and responsiveness. Zig provides powerful concurrency primitives that make it easy to write concurrent programs while maintaining safety and performance. In this tutorial, we'll explore Zig's concurrency features, including async/await, channels, and message passing.

## Async/Await

Zig's async/await syntax allows you to write asynchronous code that is easy to read and reason about. Asynchronous functions can perform I/O operations, wait for timers, or execute CPU-bound tasks without blocking the main thread.

### Writing Asynchronous Functions

To define an asynchronous function in Zig, use the `async` keyword before the function signature. Inside the function, use the `suspend` keyword to mark points where the function can yield control back to the event loop.

```zig
const std = @import("std");

// An asynchronous function that waits for a specified duration.
async fn sleep(duration: u64) void {
    // Suspend the function for the given duration.
    suspend std.time.sleep(duration * std.time.millisecond);
    // Function resumes after the sleep duration.
}
```

### Using Async/Await

You can invoke asynchronous functions using the `await` keyword, which suspends the current function until the asynchronous operation completes.

```zig
const std = @import("std");

// An example function that demonstrates async/await usage.
pub fn main() !void {
    const duration = 1000; // milliseconds
    try await sleep(duration);
    std.debug.print("Slept for {} milliseconds.\n", .{duration});
}
```

## Channels

Channels provide a safe and efficient way for different parts of a program to communicate by sending and receiving messages. Zig's channel implementation is based on the CSP (Communicating Sequential Processes) model.

### Creating and Sending Messages

You can create a channel using the `std.async.Channel` type. Channels are generic over the type of messages they can transmit.

```zig
const std = @import("std");

pub fn main() !void {
    var channel = try std.async.Channel(i32).create(std.heap.c_allocator);
    const message = 42;
    try channel.send(message);
    std.debug.print("Sent message: {}\n", .{message});
}
```

### Receiving Messages

To receive messages from a channel, use the `receive` method, which blocks until a message is available.

```zig
const std = @import("std");

pub fn main() !void {
    var channel = try std.async.Channel(i32).create(std.heap.c_allocator);
    const message = 42;
    try channel.send(message);

    const received_message = try channel.receive();
    std.debug.print("Received message: {}\n", .{received_message});
}
```

## Select

The `select` statement allows you to wait for multiple channel operations simultaneously, enabling non-blocking communication.

```zig
const std = @import("std");

pub fn main() !void {
    var channel1 = try std.async.Channel(i32).create(std.heap.c_allocator);
    var channel2 = try std.async.Channel(i32).create(std.heap.c_allocator);

    try channel1.send(1);
    try channel2.send(2);

    select {
        channel1.receive() -> message1 => {
            std.debug.print("Received from channel 1: {}\n", .{message1});
        }
        channel2.receive() -> message2 => {
            std.debug.print("Received from channel 2: {}\n", .{message2});
        }
    }
}
```

## Conclusion

Zig's concurrency features, including async/await, channels, and the select statement, empower developers to write efficient and expressive concurrent programs. By leveraging these primitives, you can build highly responsive and scalable applications while ensuring safety and correctness.

---

This tutorial covers the basics of concurrency in Zig, providing you with a solid foundation to explore more advanced topics and build sophisticated concurrent applications. Happy coding!