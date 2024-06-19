---
title: "Build a CLI Todo List using Elixir"
description: "Learn how to build a simple cli todo tool using elixir"
icon: "code"
draft: false
---

This section will guide you throguh creating a basic command-line TODO application. We will explore key conteps and syntac along the way, makin git perfect for people who just started using the language.

### Getting started

First, we should create a new project by running the command

```bash
mix new todo_cli --sup
```

We have already gone through the meaning of `--sup` in the first tutorial we had.

Our proejct will consist of four main modules:

1. `TodoCli.Application`: This module defines the application itself and utilizes the Supervisor behavior to manage child processes.
2. `TodoCli.MixProject`: This module defines the project configuration using Mix, the Elixir build tool. It specifies dependencies, the Elixir version, and the application name. For this project, you won't need to update it since we are not going to need any external dependancies.
3. `CLI`: This module handles the command-line interface (CLI) interaction. It takes user input, processes commands, and interacts with the Todo module.
4. `Todo`: This module represents the to-do list itself. It defines functions to manage tasks like adding, listing, and deleting them.

### The Todo Module

The todo module manges the Todo tasks, providing functions for adding, listing and deleting tasks. In the lib folder, create a file called `todo.ex` and put in the following code.

```elixir
#lib/todo.ex
defmodule Todo do
  defstruct tasks: []

  @doc """
  Initializes a new to-do list.
  """
  def new() do
    %Todo{}
  end

  @doc """
  Adds a task to the to-do list.
  """
  def add_task(%Todo{tasks: tasks} = todo, task) do
    %Todo{todo | tasks: tasks ++ [task]}
  end

  @doc """
  Lists all tasks in the to-do list.
  """
  def list_tasks(%Todo{tasks: tasks}) do
    tasks
  end

  @doc """
  Deletes a task from the to-do list by its index.
  """
  def delete_task(%Todo{tasks: tasks} = todo, index) when is_integer(index) and index >= 0 do
    if index < length(tasks) do
      tasks = List.delete_at(tasks, index)
      %Todo{todo | tasks: tasks}
    else
      IO.puts("Task deletion failed. Invalid index: #{index}")
      todo
    end
  end
end

```

This module defines the `Todo` struct and functions fro initializing a new Todo list, adding tasks, listing tasks, and deleting tasks.

### The CLI module

After creating the Todo module, we now move over to create the CLI module which handles user input and commands. This will be using this interface to communicate with the module and manage their tasks. In the lib folder, create a file called `cli.ex` and put in the following code.

```elixir
#lib/cli.ex
defmodule CLI do
  alias Todo

  @doc """
  Starts the CLI and processes user commands.
  """
  def start() do
    todo = Todo.new()
    loop(todo)
  end

  defp loop(todo) do
    IO.puts("Commands: add <task>, list, delete <index>, quit")
    command = IO.gets("> ") |> String.trim()

    new_todo =
      case String.split(command) do
        ["add" | task] ->
          task = Enum.join(task, " ")
          todo = Todo.add_task(todo, task)
          IO.puts("Task added.")
          todo

        ["list"] ->
          IO.puts("Tasks:")
          Enum.with_index(Todo.list_tasks(todo))
          |> Enum.each(fn {task, index} -> IO.puts("#{index + 1}. #{task}") end)
          todo

        ["delete", index_str] ->
          case Integer.parse(index_str) do
            {index, _} when index > 0 ->
              todo = Todo.delete_task(todo, index - 1)
              IO.puts("Task deleted.")
              todo

            :error ->
              IO.puts("Invalid index. Please enter a valid number.")
              todo
          end

        ["quit"] ->
          IO.puts("Goodbye!")
          :stop

        _ ->
          IO.puts("Invalid command.")
          todo
      end

    if new_todo != :stop do
      loop(new_todo)
    end
  end
end

```

This module defines the start/0 function, which initiates the CLI interface and starts the command loop. It interacts with the Todo module to perform actions such as adding, listing, and deleting tasks.

Now that we are done with creating the two modules, we are going to configure the application and test out the commands.

### Configuring our entry point

The `TodoCli.Application` module is the entry point of our application. It starts the supervision tree and initiates the CLI interface. Open the `application.ex` file in lib/todo_cli folder and update it using the following code:

```elixir
#lib/todo_cli/application.ex

defmodule TodoCli.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = []
    opts = [strategy: :one_for_one, name: TodoCli.Supervisor]
    Supervisor.start_link(children, opts)

    # Start the CLI
    CLI.start()
  end
end
```

### Running the project

We are now done creating the project. Now we are going to start the project buy running:

```elixir
mix run --no-halt
```

You should get an output like this:

![img](https://i.imgur.com/mHMmII0.png)

As you can see, we have four commands which are `add, list, delete`, and `quit`. Lets add a task by typing:

```bash
add Create a blog post
```

After adding and also running the `list` command

```bash
Tasks:
1. Create a blog post
```

If we add a second task like `Code a new feature` using the `add` command, and running `list` again, we will have:

```bash
Tasks:
1. Create a blog post
2. Code a new feature
```

Now if you are done doing a task and you want to delete it, you can use the `delete` command with the number of the task you want to delete. In this case, if we want to delete the second task, we can run:

```bash
delete 2
```

and the out after running `list` again will be:

````bash
```bash
Tasks:
1. Create a blog post
````

The last command `quit` will simply quit the application.

This brings us to the end of this session, you can use this CLI tool and adapt it for your specific usecase.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
