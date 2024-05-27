---
title: "Modules and functions"
description: "In this section, we will define our own modules, with different levels of complexity. As our examples get longer in size, it can be tricky to type them all in the shell. It's about time for us to learn how to compile Elixir code and also how to run Elixir scripts."
icon: "code"
draft: false
---

In Elixir, we group several functions into modules. We've already used many different modules in the previous chapters, such as the String module:

```bash
String.length("hello")
5
```

In order to create our own modules in Elixir, we use the `defmodule` macro. The first letter of the module must be in uppercase. We use the `def` macro to define functions in that module. The first letter of every function must be in lowercase (or underscore):

```bash
defmodule Math do
  def sum(a, b) do
    a + b
  end
end

Math.sum(1, 2)
3
```

### Compilation

Most of the time it is convenient to write modules into files so they can be compiled and reused. Let's assume we have a file named `math.ex` with the following contents:

```bash
defmodule Math do
  def sum(a, b) do
    a + b
  end
end
```

This file can be compiled using elixirc:

```bash
elixirc math.ex
```

This will generate a file named `Elixir.Math.beam` containing the bytecode for the defined module. If we start `iex` again, our module definition will be available (provided that `iex` is started in the same directory the bytecode file is in):

```bash
Math.sum(1, 2)
3
```

Elixir projects are usually organized into three directories:

- `_build` - contains compilation artifacts
- `lib` - contains Elixir code (usually `.ex` files)
- `test` - contains tests (usually `.exs` files)

When working on actual projects, the build tool called `mix` will be responsible for compiling and setting up the proper paths for you. For learning and convenience purposes, Elixir also supports a scripting mode which is more flexible and does not generate any compiled artifacts.

### Scripting mode

In addition to the Elixir file extension `.ex`, Elixir also supports `.exs` files for scripting. Elixir treats both files exactly the same way, the only difference is in intention. `.ex` files are meant to be compiled while `.exs` files are used for scripting. This convention is followed by projects like `mix`.

For instance, we can create a file called `math.exs`:

```bash
defmodule Math do
  def sum(a, b) do
    a + b
  end
end

IO.puts Math.sum(1, 2)
```

and execute it as:

```bash
elixir math.exs
```

Because we used `elixir` instead of `elixirc`, the module was compiled and loaded into memory, but no `.beam` file was written to disk. In the following examples, we recommend you write your code into script files and execute them as shown above.
