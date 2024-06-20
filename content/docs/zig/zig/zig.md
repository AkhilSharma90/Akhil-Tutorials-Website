---
title: "A getting started guide to Zig"
description: "Zig is a general-purpose programming language and toolchain for maintaining robust, optimal, and reusable software."
icon: "code"
draft: false
---

## Key Features of Zig:
- Safety and Performance: Zig ensures memory safety and offers performance comparable to languages like C.
- Simplicity and Maintainability: Zig's focus on avoiding hidden control flow and minimizing dependencies.
- Comptime: Fast compile-time execution and it sets Zig apart from other languages.

## Installation
This guide assumes Zig 0.11, which is the latest major release as of writing.
1. Download and extract a prebuilt master binary of Zig from: [https://ziglang.org/download/](https://ziglang.org/download/)
   
2. Add Zig to your path
    - **linux, macos, bsd**
    Add the location of your Zig binary to your PATH environment variable. For an installation, add `export PATH=$PATH:~/zig` or similar to your `/etc/profile` (system-wide) or `$HOME/.profile`. If these changes do not apply immediately, run the line from your shell.
    
    - **windows**
        a) System wide (admin powershell)
        ```powershell
        [Environment]::SetEnvironmentVariable(
            "Path",
            [Environment]::GetEnvironmentVariable("Path", "Machine") + ";C:\your-path\zig-windows-x86_64-your-version",
            "Machine"
        )
        ```

        b) User level (powershell)
        ```powershell
        [Environment]::SetEnvironmentVariable(
            "Path",
            [Environment]::GetEnvironmentVariable("Path", "User") + ";C:\your-path\zig-windows-x86_64-your-version",
            "User"
        )
        ```

    Close your terminal and create a new one.
    
3. Verify your installation with `zig version`. The output should look like this:
   ```
   $ zig version
   0.11
   ```

## Hello World
Create a file called `main.zig`, with the following contents:

```zig
const std = @import("std");

pub fn main() void {
    std.debug.print("Hello, {s}!\n", .{"World"});
}
```

Use `zig run main.zig` to build and run it. In this example, `Hello, World!` will be written to stderr, and is assumed to never fail.

## Assignment
Value assignment has the following syntax: `(const|var) identifier[: type] = value`.
- `const` indicates that `identifier` is a constant that stores an immutable value.
- `var` indicates that `identifier` is a variable that stores a mutable value.
- `: type` is a type annotation for `identifier`, and may be omitted if the data type of `value` can be inferred.
```zig
const constant: i32 = 5;  // signed 32-bit constant
var variable: u32 = 5000; // unsigned 32-bit variable
const inferred_constant = @as(i32, 5);
var inferred_variable = @as(u32, 5000);
```
Constants and variables must have a value. If no known value can be given, the undefined value, which coerces to any type, may be used as long as a type annotation is provided.
```zig
const a: i32 = undefined;
var b: u32 = undefined;
```
Where possible, const values are preferred over var values.

## Arrays
Arrays are denoted by `[N]T`, where `N` is the number of elements in the array and `T` is the type of those elements (i.e., the array’s child type). For array literals, `N` may be replaced by `_` to infer the size of the array.
```zig
const a = [5]u8{ 'h', 'e', 'l', 'l', 'o' };
const b = [_]u8{ 'w', 'o', 'r', 'l', 'd' };
```
To get the size of an array, simply access the array’s `len` field.
```zig
const array = [_]u8{ 'h', 'e', 'l', 'l', 'o' };
const length = array.len; // 5
```

## If
Zig’s if statements only accept bool values (i.e. true or false). There is no concept of truthy or falsy values.
Here, we will introduce testing. Save the below code and compile + run it with `zig test file-name.zig`. We will be using the `expect` function from the standard library, which will cause the test to fail if it’s given the value `false`. When a test fails, the error and stack trace will be shown.
```zig
const expect = @import("std").testing.expect;

test "if statement" {
    const a = true;
    var x: u16 = 0;

    if (a) {
        x += 1;
    } else {
        x += 2;
    }

    try expect(x == 1);
}

// If statements also work as expressions.
test "if statement expression" {
    const a = true;
    var x: u16 = 0;

    x += if (a) 1 else 2;
    try expect(x == 1);
}
```

## While
Zig’s while loop has three parts - a condition, a block, and a continue expression. Without a continue expression.
```zig
test "while" {
    var i: u8 = 2;

    while (i < 100) {
        i *= 2;
    }
    try expect(i == 128);
}
```
With a continue expression.
```zig
test "while with continue expression" {
    var sum: u8 = 0;
    var i: u8 = 1;

    while (i <= 10) : (i += 1) {
        sum += i;
    }

    try expect(sum == 55);
}
```
With a continue.
```zig
test "while with continue" {
    var sum: u8 = 0;
    var i: u8 = 0;

    while (i <= 3) : (i += 1) {
        if (i == 2) continue;
        sum += i;
    }

    try expect(sum == 4);
}
```
With a break.
```zig
test "while with break" {
    var sum: u8 = 0;
    var i: u8 = 0;

    while (i <= 3) : (i += 1) {
        if (i == 2) break;
        sum += i;
    }

    try expect(sum == 1);
}
```

## For
For loops are used to iterate over arrays. For loops follow this syntax. Like while, for loops can use break and continue. Here, we’ve had to assign values to _, as Zig does not allow us to have unused values.
```zig
test "for" {
   //character literals are equivalent to integer literals
   const string = [_]u8{ 'a', 'b', 'c' };

   for (string, 0..) |character, index| {
       _ = character;
       _ = index;
   }

   for (string) |character| {
       _ = character;
   }

   for (string, 0..) |_, index| {
       _ = index;
   }

   for (string) |_| {}
}
```

## Functions
All function arguments are immutable - if a copy is desired the user must explicitly make one. Unlike variables, which are snake_case, functions are camelCase. Here’s an example of declaring and calling a simple function.
```zig
fn addFive(x: u32) u32 {
   return x + 5;
}

test "function" {
   const y = addFive(0);
   try expect(@TypeOf(y) == u32);
   try expect(y == 5);
}
```
Recursion is allowed:
```zig
fn fibonacci(n: u16) u16 {
   if (n == 0 or n == 1) return n;
   return fibonacci(n - 1) + fibonacci(n - 2);
}

test "function recursion" {
   const x = fibonacci(10);
   try expect(x == 55);
}
```
## Values Ignoring
Values can be ignored using `_` instead of a variable or const declaration. This does not work at the global scope (i.e., it only works inside functions and blocks) and is useful for ignoring the values returned from functions if you do not need them.
```zig
_ = 10;
```

## Defer
Defer is used to execute a statement while exiting the current block.
```zig
test "defer" {
   var x: i16 = 5;
   {
       defer x += 2;
       try expect(x == 5);
   }
   try expect(x == 7);
}
```
When there are multiple defers in a single block, they are executed in reverse order.
```zig
test "multi defer" {
   var x: f32 = 5;
   {
       defer x += 2;
       defer x /= 2;
   }
   try expect(x == 4.5);
}
```

## Errors
An error set is like an enum, where each error in the set is a value. There are no exceptions in Zig; errors are values.
```zig
const FileOpenError = error{
   AccessDenied,
   OutOfMemory,
   FileNotFound,
};
```

## Switch
Zig’s switch works as both a statement and an expression. The types of all branches must coerce to the type which is being switched upon. All possible values must have an associated branch - values cannot be left out. Cases cannot fall through to other branches.
```zig
test "switch statement" {
   var x: i8 = 10;

   switch (x) {
       -1...1 => {
           x = -x;
       },
       10, 100 => {
           //special considerations must be made
           //when dividing signed integers
           x = @divExact(x, 10);
       },
       else => {},
   }

   try expect(x == 1);
}
```
Here is the former, but as a switch expression.
```zig
test "switch expression" {
   var x: i8 = 10;
   x = switch (x) {
       -1...1 => -x,
       10, 100 => @divExact(x, 10),
       else => x,
   };

   try expect(x == 1);
}
```
## Slices
Slices can be thought of as a pair of [*]T (the pointer to the data) and a usize (the element count). Their syntax is []T, with T being the child type.
```zig
fn total(values: []const u8) usize {
   var sum: usize = 0;
   for (values) |v| sum += v;
   return sum;
}

test "slices" {
   const array = [_]u8{ 1, 2, 3, 4, 5 };
   const slice = array[0..3];
   try expect(total(slice) == 6);
}
```

## Enums
Zig’s enums allow you to define types with a restricted set of named values.
```zig
const Direction = enum { north, south, east, west };
```

## Structs
Structs are Zig’s most common kind of composite data type, allowing you to define types that can store a fixed set of named fields.
```zig
const Vec3 = struct { x: f32, y: f32, z: f32 };

test "struct usage" {
   const my_vector = Vec3{
       .x = 0,
       .y = 100,
       .z = 50,
   };

   _ = my_vector;
}
```
## ArrayList
The std.ArrayList is commonly used throughout Zig, serving as a buffer that can change in size. std.ArrayList(T) is similar to C++’s std::vector<T> and Rust’s Vec<T>.
```zig
const eql = std.mem.eql;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;

test "arraylist" {
   var list = ArrayList(u8).init(test_allocator);
   defer list.deinit();

   try list.append('H');
   try list.append('e');
   try list.append('l');
   try list.append('l');
   try list.append('o');
   try list.appendSlice(" World!");
   try expect(eql(u8, list.items, "Hello World!"));
}
```

## Filesystem
Creating, opening, writing to, and reading from a file in the current working directory.
```zig
test "createFile, write, seekTo, read" {
   const file = try std.fs.cwd().createFile(
       "junk_file.txt",
       .{ .read = true },
   );

   defer file.close();

   const bytes_written = try file.writeAll("Hello File!");
   _ = bytes_written;

   var buffer: [100]u8 = undefined;

   try file.seekTo(0);
   const bytes_read = try file.readAll(&buffer);
   try expect(eql(u8, buffer[0..bytes_read], "Hello File!"));
}
```

## Threads
Using std.Thread for utilizing OS threads.
```zig
fn ticker(step: u8) void {
   while (true) {
       std.time.sleep(1 * std.time.ns_per_s);
       tick += @as(isize, step);
   }
}

var tick: isize = 0;

test "threading" {
   var thread = try std.Thread.spawn(.{}, ticker, .{@as(u8, 1)});
   _ = thread;
   try expect(tick == 0);
   std.time.sleep(3 * std.time.ns_per_s / 2);
   try expect(tick == 1);
}
```

## Sorting
The standard library provides utilities for in-place sorting slices.
```zig
test "sorting" {
   var data = [_]u8{ 10, 240, 0, 0, 10, 5 };

   std.mem.sort(u8, &data, {}, comptime std.sort.asc(u8));
   try expect(eql(u8, &data, &[_]u8{ 0, 0, 5, 10, 10, 240 }));
   std.mem.sort(u8, &data, {}, comptime std.sort.desc(u8));
   try expect(eql(u8, &data, &[_]u8{ 240, 10, 10, 5, 0, 0 }));
}
```

## Async
Zig’s async functions allow for asynchronous execution without the need for OS threads.
```zig
const expect = @import("std").testing.expect;

var foo: i32 = 1;
test "suspend with no resume" {
   var frame = async func(); //1
   _ = frame;
   try expect(foo == 2);     //4
}

fn func() void {
   foo += 1;                 //2
   suspend {}                //3
   foo += 1;                 //never reached!
}

var bar: i32 = 1;

test "suspend with resume" {
   var frame = async func2();  //1
   resume frame;               //4
   try expect(bar == 3);       //6
}

fn func2() void {
   bar += 1;                   //2
   suspend {}                  //3
   bar += 1;                   //5
}
```

## Async / Await
Async functions in Zig can be invoked with the `await` keyword to wait for their completion and retrieve their return value asynchronously.
```zig
fn func3() u32 {
   return 5;
}

test "async / await" {
   var frame = async func3();
   try expect(await frame == 5);
}
```

Using `await` on an async function from another async function allows for chaining asynchronous operations.
```zig
fn asyncOperation() u32 {
   return 10;
}

fn asyncOperation2(value: u32) u32 {
   return value * 2;
}

test "chaining async operations" {
   var frame = async asyncOperation();
   var result = await asyncOperation2(await frame);
   try expect(result == 20);
}
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).