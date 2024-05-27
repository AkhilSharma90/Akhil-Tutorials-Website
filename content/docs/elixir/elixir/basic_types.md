---
title: "Basic types"
description: "In this section, we will learn more about Elixir basic types: integers, floats, booleans, atoms, and strings."
icon: "code"
draft: false
---

```bash
1          # integer
0x1F       # integer
1.0        # float
true       # boolean
:atom      # atom / symbol
"elixir"   # string
[1, 2, 3]  # list
{1, 2, 3}  # tuple
```

### Basic Arithmetic

Open up `iex` and type the following expressions:
```bash
1 + 2
3
5 * 5
25
10 / 2
5.0
```

Notice that `10 / 2` returned a float `5.0` instead of an integer `5`. This is expected. In Elixir, the operator `/` always returns a float. If you want to do integer division or get the division remainder, you can invoke the `div` and `rem` functions:

```bash
div(10, 2)
5
div 10, 2
5
rem 10, 3
1
```

Notice that Elixir allows you to drop the parentheses when invoking functions that expect one or more arguments. This feature gives a cleaner syntax when writing declarations and control-flow constructs. However, Elixir developers generally prefer to use parentheses.

Elixir also supports shortcut notations for entering binary, octal, and hexadecimal numbers:

```bash
0b1010
10
0o777
511
0x1F
31
```

Float numbers require a dot followed by at least one digit and also support e for scientific notation:

```bash
1.0
1.0
1.0e-10
1.0e-10
```

Floats in Elixir are 64-bit precision.

You can invoke the round function to get the closest integer to a given float, or the trunc function to get the integer part of a float.

```bash
round(3.58)
4
trunc(3.58)
3
```
Finally, we work with different data types, we will learn Elixir provides several predicate functions to check for the type of a value. For example, the is_integer can be used to check if a value is an integer or not:
```bash
is_integer(1)
true
is_integer(2.0)
false
```
You can also use `is_float` or `is_number` to check, respectively, if an argument is a float, or either an integer or float.

