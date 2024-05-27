---
title: "Lists and tuples"
description: "In this section, we will learn two of the most used collection data-types in Elixir: lists and tuples."
icon: "code"
draft: false
---

### Linked Lists

Elixir uses square brackets to specify a list of values. Values can be of any type:

```bash
[1, 2, true, 3]
[1, 2, true, 3]
length([1, 2, 3])
3
```

Two lists can be concatenated or subtracted using the `++/2` and `--/2` operators respectively:

```bash
[1, 2, 3] ++ [4, 5, 6]
[1, 2, 3, 4, 5, 6]
[1, true, 2, false, 3, true] -- [true, false]
[1, 2, 3, true]
```

List operators never modify the existing list. Concatenating to or removing elements from a list returns a new list. We say that Elixir data structures are immutable. One advantage of immutability is that it leads to clearer code. You can freely pass the data around with the guarantee no one will mutate it in memory - only transform it.

Throughout the tutorial, we will talk a lot about the head and tail of a list. The head is the first element of a list and the tail is the remainder of the list. They can be retrieved with the functions `hd/1` and `tl/1`. Let's assign a list to a variable and retrieve its head and tail:

```bash
list = [1, 2, 3]
hd(list)
1
tl(list)
[2, 3]
```

Getting the head or the tail of an empty list throws an error:

```bash
hd([])
** (ArgumentError) argument error
```

Sometimes you will create a list and it will return a quoted value preceded by `~c`. For example:

```bash
[11, 12, 13]
~c"\v\f\r"
[104, 101, 108, 108, 111]
~c"hello"
```

When Elixir sees a list of printable ASCII numbers, Elixir will print that as a charlist (literally a list of characters). Charlists are quite common when interfacing with existing Erlang code. Whenever you see a value in IEx and you are not quite sure what it is, you can use the i/1 to retrieve information about it:

```bash
i ~c"hello"
Term
  i ~c"hello"
Data type
  List
Description
  ...
Raw representation
  [104, 101, 108, 108, 111]
Reference modules
  List
Implemented protocols
  ...
```

### Tuples
Elixir uses surly brackets to define tuples. Like lists, tuples can hold any value:
```bash
{:ok, "hello"}
{:ok, "hello"}
tuple_size({:ok, "hello"})
2
```

Tuples store elements contiguously in memory. This means accessing a tuple element by index or getting the tuple size is a fast operation. Indexes start from zero:

```bash
tuple = {:ok, "hello"}
{:ok, "hello"}
elem(tuple, 1)
"hello"
tuple_size(tuple)
2
```
It is also possible to put an element at a particular index in a tuple with put_elem/3

```bash
tuple = {:ok, "hello"}
{:ok, "hello"}
put_elem(tuple, 1, "world")
{:ok, "world"}
tuple
{:ok, "hello"}
```
Notice that put_elem/3 returned a new tuple. The original tuple stored in the tuple variable was not modified. Like lists, tuples are also immutable. Every operation on a tuple returns a new tuple, it never changes the given one.

### List or tuples?
What is the difference between lists and tuples?

Lists are stored in memory as linked lists, meaning that each element in a list holds its value and points to the following element until the end of the list is reached. This means accessing the length of a list is a linear operation: we need to traverse the whole list in order to figure out its size.