---
title: "Pattern matching"
description: "In this section, we will learn why the `=` operator in Elixir is called the match operator and how to use it to pattern match inside data structures. We will learn about the pin operator `^`used to access previously bound values."
icon: "code"
draft: false
---

### The match operator

We have used the `=` operator a couple times to assign variables in Elixir:
```bash
x = 1
1
x
1
```
In Elixir, the = operator is actually called the match operator. Let's see why:
```bash
x = 1
1
1 = x
1
2 = x
** (MatchError) no match of right hand side value: 1
```

Notice that `1 = x` is a valid expression, and it matched because both the left and right side are equal to `1`. When the sides do not match, a MatchError is raised.

A variable can only be assigned on the left side of `=`:
```bash
1 = unknown
** (CompileError) iex:1: undefined variable "unknown"
```

### Pattern matching
The match operator is not only used to match against simple values, but it is also useful for destructuring more complex data types. For example, we can pattern match on tuples:
```bash
{a, b, c} = {:hello, "world", 42}
{:hello, "world", 42}
a
:hello
b
"world"
```
