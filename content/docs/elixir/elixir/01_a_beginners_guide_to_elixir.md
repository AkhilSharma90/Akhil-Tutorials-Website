---
title: "A beginner's guide to the Elixir programming language"
description: "Elixir is a process-oriented, functional programming language that runs on the Erlang virtual machine (BEAM). The language was influenced by Ruby. This inspiration can be seen and felt in Elixir’s ecosystem and tooling options. Elixir is known to be easy to learn and widely applicable within the software development industry."
icon: "code"
draft: false
---

In this section, we will cover:
- What is Elixir?
- Key features, tools, and uses of Elixir
- Intro to Elixir functional programming
- Simple code snippets

### What is Elixir?
Elixir is a general-purpose, functional, concurrent programming language designed for building applications that are reliable, scalable, and easy to maintain. Tt looks a lot like Ruby but offers features that help with handling lots of tasks at the same time (concurrency), recovering from errors quickly (fault tolerance), and low latency.

### Key features, tools, and uses of Elixir

Elixer has many cool features such as:
- Elixir compiles to bytecode for the Erlang VM making it very efficient.
- Metaprogramming with macros and polymorphism via protocols that saves time and effort.
- Emphasis on higher-order functions and recursion
- Handle large data collections efficiently with lazy and asynchronous operations.
- Pattern matching which makes it easy to work with complex data.

The language also has a solid set of web development tools such as:

- Mix: Mix is a build tool that allows you to create projects, run tests, manage tasks, and much more.
- IEx: IEx, Elixir’s interactive shell, provides you with many features like auto-complete, debugging, code reloading, and more.
- Phoenix: Phoenix is known to be one of the best web frameworks. It’s based on the MVC architecture just like Ruby on Rails.

Elixir is great for web applications of any size, web APIs (such as JSON or GraphQL), event-driven systems, distributed systems, internet of things, embedded systems, and much more. 

### Intro to Elixir functional programming

Elixir is a functional programming language, which means it helps you write clear and efficient code. Here are some key concepts like:

- Immutability: In Elixir, once a value is created, it cannot be changed. This makes your code more predictable and easier to run in parallel.
- Functions: Functions are the main building blocks. In functional programming, pure functions are preferred because they use immutable values, depend only on their arguments, don’t have side effects beyond their return values. Impure functions are more complex and can have unpredictable results. In Elixir, functions can be passed around as arguments and return values, making the code very flexible.
- Declarative Code: Instead of focusing on how to solve a problem, you focus on what needs to be done. This makes your code more concise and easier to understand, leading to fewer bugs.
By grasping these principles, you'll be able to use Elixir to build efficient, reliable applications.

### Some basic elixir code examples
These are just examples of elixir basic code snippets.

**Strings**

Elixir uses UTF-8 to encode strings. UTF-8 is a variable-width character encoding that uses one to four eight-bit bytes to store each code point. Strings are surrounded by double quotes, like ”this”. Let’s take a look at a simple Hello, World! in Elixir:
```elixir
IO.puts("Hello, World!")
```


**Atoms**

Atoms are constants whose values are their own names. In other languages, they are called symbols. They’re typically used to enumerate over distinct values:
```elixir
iex> :cat
:cat
iex> :dog
:dog
iex> :fish
:fish
```


**Booleans**

Elixir supports the booleans true and false:
```elixir
iex> true
true 
iex> true == false
false
```


**Arithmetic operations**

You can also do some basic arithmetic operations
```elixir
iex> 2 + 2
4
iex> 10 * 2
20
```
and the divide operator `/` always returns as a float:
```elixir
iex> 8 / 2
4.0
```


**Modules and functions**

In Elixir, functions are grouped into modules. An example of a module is the String module. Here’s an example:
```elixir
iex> String.length("elixir")
6
```
Looking more into functions, we also have `anonymos functions`. They start with `fn` and end with `end`.
```elixir
iex> add = fn a, b -> a+ b end
iex> add.(1,2)

#and the answer should be 3
3
```

Note that a dot (`.`) between the variable and parentesis is required to invoke an anonymouse function.

In Elixir, functions are first class citizens meaning that they can be passed as arguments to other functions the same way integers and strings can.
```elixir
iex> is_function(add)
true
```
This uses the inbuilt function `is_function` which checks to see if the parameter passed is a function and returns a bool.
Anonymous functions are closures (named functions are not) and as such they can access variables that are in scope when the function is defined. You can define a new anonymous function that uses the add anonymous function we have previously defined:


With `modules` you're able to group several functions together. Most of the time it is convenient to write modules into files so they can be compiled and reused.
Get started by creating a file named math.ex, open it in your text editor and add the following code:
```elixir
defmodule Math do
  def sum(a, b) do
    a + b
  end
end
```

In order to create your own modules in Elixir, use the defmodule macro, then use the def macro to define functions in that module. So in this case the module is Math and the function is sum.
Once this is saved the file can be compiled by typing elixirc into the terminal followed by the file name.
`$ elixirc math.ex`

This will generate a file named Elixir.Math.beam containing the bytecode for the defined module. If we start iex again, our module definition will be available (provided that iex is started in the same directory the bytecode file is in):
```elixir
iex> Math.sum(1,2)
3
```

### Creating your first Elixir project
We will be using mix and we've already discussed what mix is. Now we are going to initialise a new project by running `mix new [project_name]`. Let's choose our project name to be animals, then we will have
```bash
mix new animals
```
after that, you can open the new animals project with your desired editor and start working.

Now that you have learnt what elixir it, you can start experimenting and building simple, small projects that will further drill in these skills you learn and help you grasp them.

Open up `animal.ex` file in the lib directory. You should see some boilerplate code looking like this:
```elixir
defmodule Animals do
  @moduledoc """
  Documentation for `Animals`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Animals.hello()
      :world

  """
  def hello do
    :world
  end
end
```

Elixir has created a module with the name of your project along with a function that prints out a :world atom when called. It's also added boilerplate for module and function documentation - the first part of the file. (we will go into more detail about documentation later)

**Running the code**

Let's test out the code by running
```bash
iex -S mix
```
Which will start and compile your project, now run
```bash
Animals.hello
# :world
```