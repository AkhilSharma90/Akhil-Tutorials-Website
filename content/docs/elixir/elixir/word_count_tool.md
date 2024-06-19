---
title: "File Word Counter"
description: "Elixir is a process-oriented, functional programming language that runs on the Erlang virtual machine (BEAM). The language was influenced by Ruby. This inspiration can be seen and felt in Elixir’s ecosystem and tooling options. Elixir is known to be easy to learn and widely applicable within the software development industry."
icon: "code"
draft: false
---

This tool will read a text file and count the number of words in it. This project is straightforward and a great way to practice file handling and basic string manipulation in Elixir. You can further adjust this tool to your usecase, like creating your own editor that will have a word count.

Here's an outline of what we'll cover:

## Overview

We'll create a CLI tool named `word_counter` that reads a specified text file and counts the number of words in it. This tool will help users quickly determine the word count of a given file.

## Setting Up the Project

1. Create a new Elixir project:

```sh
mix new word_counter
cd word_counter
```

## Reading the File

Create a module `WordCounter` in `lib/word_counter.ex` to handle file reading:

```elixir
defmodule WordCounter do
    def read_file(file_path) do
    case File.read(file_path) do
        {:ok, content} -> {:ok, content}
        {:error, reason} -> {:error, reason}
    end
    end
end
```

## Counting Words

Add a function to count words in the `WordCounter` module in `lib/word_counter.ex`:
```elixir
defmodule WordCounter do
  def read_file(file_path) do
    case File.read(file_path) do
      {:ok, content} -> {:ok, content}
      {:error, reason} -> {:error, reason}
    end
  end

  def count_words(content) do
    content
    |> String.split(~r/\s+/)
    |> length()
  end
end
````

## Handling Command-line Arguments

Create a module `WordCounter.CLI` in `lib/word_counter/cli.ex` to handle command-line interaction:

```elixir
defmodule WordCounter.CLI do
  def main(args) do
    case parse_args(args) do
      {:ok, file_path} -> process_file(file_path)
      {:error, message} -> IO.puts(message)
    end
  end

  defp parse_args([file_path]) when is_binary(file_path), do: {:ok, file_path}
  defp parse_args(_), do: {:error, "Usage: word_counter <file_path>"}

  defp process_file(file_path) do
    case WordCounter.read_file(file_path) do
      {:ok, content} ->
        word_count = WordCounter.count_words(content)
        IO.puts("The file contains #{word_count} words.")

      {:error, reason} ->
        IO.puts("Failed to read file: #{reason}")
    end
  end
end

```

Modify the `mix.exs` file to set the `escript` options so that the project can be run as a standalone executable:

```elixir
defmodule WordCounter.MixProject do
    use Mix.Project

    def project do
    [
        app: :word_counter,
        version: "0.1.0",
        elixir: "~> 1.11",
        start_permanent: Mix.env() == :prod,
        deps: deps(),
        escript: escript()
    ]
    end

    defp escript do
    [main_module: WordCounter.CLI]
    end

    defp deps do
    []
    end
end
```

## Running the program

Even though we don't have any dependencies in this project, it's always a good idea to run this command to ensure everything is up-to-date:

```sh
mix deps.get
```

Now, running this command will produce and executable for the tool.

```sh
mix escript.build
```

After that, run the executable with the path to your file. For this case, simple create a file named `demo.txt` on your root project, and run the following command:

```sh
./word_counter demo.txt
```

You should see and output like:

```bash
The file contains 2 words.
```

## Conclusion

You've built a simple and useful CLI tool in Elixir that reads a text file and counts the number of words. This project demonstrates basic file handling, string manipulation, and command-line interaction in Elixir.


### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
