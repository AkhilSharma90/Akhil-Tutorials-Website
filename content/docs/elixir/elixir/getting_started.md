---
title: "Getting Started"
description: "This guide will teach you about Elixir fundamentals - the language syntax, how to define modules, the common data structures in the language, and more. This chapter will focus on ensuring that Elixir is installed and that you can successfully run Elixir's Interactive Shell, called IEx."
icon: "code"
draft: false
---

### How to Install Elixir

Elixir is a dynamic, functional programming language designed for building scalable and maintainable applications. This guide will walk you through the steps to install and use elixir. For a detailed install, check out this: [installation guide](https://elixir-lang.org/install.html).

### Interactive mode

When you install Elixir, you will habe three new command line executables: `iex`, `elixir` and `elixirc`.

For now, let's start by running iex (or iex.bat if you are on Windows PowerShell, where iex is a PowerShell command) which stands for Interactive Elixir. In interactive mode, we can type any Elixir expression and get its result. Let's warm up with some basic expressions.

Open up iex and type the following expressions:

```bash
Erlang/OTP 26 [64-bit] [smp:2:2] [...]

Interactive Elixir - press Ctrl+C to exit
40 + 2
42
"hello" <> " world"
"hello world"
```
To exit, press `Ctrl+C` twice.

### Running scripts

After getting familiar with the basics of the language you may want to try writing simple programs. This can be accomplished by putting the following Elixir code into a file:

```bash
IO.puts("Hello world from Elixir")
```

Save it as `simple.exs` and execute it with `elixir`:
```bash
elixir simple.exs
Hello world from Elixir
```