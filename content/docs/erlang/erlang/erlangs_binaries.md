---
title: "Binaries in Erlang Programming Language"
description: ""
icon: "code"
draft: false
---

Erlang provides a data structure called a binary to store large quantities of raw data efficiently. Binaries are more space-efficient compared to lists or tuples, and the Erlang runtime system is optimized for efficient input and output operations involving binaries.

## What are Binaries?

Binaries in Erlang are written and printed as sequences of integers or strings, enclosed in double less than and greater than brackets (`<< >>`). They are used for handling raw data efficiently.

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    io:fwrite("~p~n", [<<5, 10, 20>>]),
    io:fwrite("~p~n", [<<"hello">>]).
```

### Output

When you run the above program, the output will be:

```plaintext
<<5,10,20>>
<<"hello">>
```

## Functions for Working with Binaries

Erlang provides several functions to work with binaries. Below is a summary of some essential functions and their descriptions:

| Sr.No. | Method            | Description                                          |
|--------|-------------------|------------------------------------------------------|
| 1      | `list_to_binary/1`  | Converts an existing list to a binary.              |
| 2      | `split_binary/2`    | Splits the binary based on the specified index.     |
| 3      | `term_to_binary/1`  | Converts a term to binary.                          |
| 4      | `is_binary/1`       | Checks if a bitstring is a binary value.            |
| 5      | `binary_part/2`     | Extracts a part of the binary.                      |
| 6      | `binary_to_float/1` | Converts a binary value to a float.                 |
| 7      | `binary_to_integer/1`| Converts a binary value to an integer.             |
| 8      | `binary_to_list/1`  | Converts a binary value to a list.                  |
| 9      | `binary_to_atom/1`  | Converts a binary value to an atom.                 |

## Examples of Using Binary Functions

### Converting a List to a Binary

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    List = [1, 2, 3, 4, 5],
    Binary = list_to_binary(List),
    io:fwrite("Binary: ~p~n", [Binary]).
```

### Splitting a Binary

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    Binary = <<1, 2, 3, 4, 5>>,
    {FirstPart, SecondPart} = split_binary(Binary, 2),
    io:fwrite("First part: ~p~nSecond part: ~p~n", [FirstPart, SecondPart]).
```

### Converting a Term to Binary

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    Term = {hello, world},
    Binary = term_to_binary(Term),
    io:fwrite("Binary: ~p~n", [Binary]).
```

### Checking if a Value is Binary

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    Value = <<1, 2, 3>>,
    IsBinary = is_binary(Value),
    io:fwrite("Is binary: ~p~n", [IsBinary]).
```

### Extracting a Part of a Binary

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    Binary = <<1, 2, 3, 4, 5>>,
    Part = binary_part(Binary, {1, 3}),
    io:fwrite("Binary part: ~p~n", [Part]).
```

### Converting a Binary to a Float

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    Binary = <<64, 9, 33, 251, 84, 68, 45, 24>>,
    Float = binary_to_float(Binary),
    io:fwrite("Float: ~p~n", [Float]).
```

### Converting a Binary to an Integer

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    Binary = <<1, 0, 0, 0>>,
    Integer = binary_to_integer(Binary),
    io:fwrite("Integer: ~p~n", [Integer]).
```

### Converting a Binary to a List

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    Binary = <<"hello">>,
    List = binary_to_list(Binary),
    io:fwrite("List: ~p~n", [List]).
```

### Converting a Binary to an Atom

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    Binary = <<"hello">>,
    Atom = binary_to_atom(Binary, utf8),
    io:fwrite("Atom: ~p~n", [Atom]).
```

## Conclusion

Erlang binaries are efficient for storing and manipulating large amounts of raw data. By using the functions provided by Erlang's standard library, you can perform various operations on binaries, such as conversion, splitting, and extraction.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
