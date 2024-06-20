---
title: "Your First Rust Program"
description: "Learn to write, compile, and run your first simple Rust program. Understand the basics of Rust compilation and execution."
icon: "code"
draft: false
---


Welcome to your first adventure in Rust programming! In this post, we'll take a detailed walk through the process of writing, compiling, and executing a simple Rust program. We'll cover everything you need to know to get started, from setting up your project to understanding each line of code.

## Getting Started with Rust

Before we dive into writing code, make sure you have Rust installed. If you haven't installed Rust yet, refer to the previous post on [Getting Started with Rust](#).

### Step 1: Create Your Project

First, we need to set up a new Rust project. Rust projects are created and managed with Cargo, Rust’s package manager and build system.

Open your terminal or command prompt and run the following command:
```bash
cargo new hello_rust
cd hello_rust
```
This command creates a new directory called `hello_rust` which contains all necessary files for a Rust project:
- `Cargo.toml`: The manifest file for your project. This file contains metadata and dependencies of your project.
- `src`: A directory containing your source files, starting with `main.rs`.

### Step 2: Write Your First Rust Program

Navigate to the `src` directory and open the `main.rs` file in your favorite text editor. You will see that Cargo has already placed a simple program there for you:
```rust
fn main() {
    println!("Hello, World!");
}
```
Let's break this down:
- `fn main() { ... }`: This defines the main function, which is the entry point of every Rust program. All Rust programs start executing from the `main` function.
- `println!("Hello, World!");`: This line of code uses the `println!` macro to print text to the console. In Rust, macros are denoted by the `!`.

### Step 3: Understanding What You Wrote

The `println!` macro is very powerful and commonly used in Rust for printing output. It can print not just simple strings, but also formatted data. For example:
```rust
let name = "Rust";
println!("Hello, {}!", name);
```
This will print "Hello, Rust!" where `{}` is replaced by the value of `name`.

### Step 4: Compile and Run Your Program

Now that you've written your program, it's time to compile and run it:
```bash
cargo run
```
When you execute this command, several things happen:
- **Compilation:** Cargo checks your code for errors and compiles it into a binary executable.
- **Execution:** If the compilation is successful, Cargo then runs the binary executable.

You should see the output:
```
Hello, World!
```

## Conclusion

Congratulations! You've just written, compiled, and run your first Rust program. This simple example has introduced you to the basics of Rust projects, the structure of a Rust program, and how to use Cargo to manage and run Rust code.

In our next post, we'll explore more about Rust's variable bindings, types, and operations, which will help you write more complex programs. Stay tuned and happy coding!

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).