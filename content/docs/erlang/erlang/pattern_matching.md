---
title: "Pattern Matching in Erlang"
description: ""
icon: "code"
draft: false
---

Pattern matching is a fundamental concept in Erlang that allows you to destructure data structures, bind variables, and control the flow of your program. This tutorial will guide you through the basics of pattern matching, including how to use it with different data types and the rules governing pattern matching success and failure.

## Introduction to Pattern Matching

In Erlang, patterns look similar to terms. They can be simple literals like atoms and numbers, or compound structures like tuples and lists. Patterns can also contain variables, which are alphanumeric strings starting with a capital letter or an underscore. The special "anonymous variable" `_` is used when you want to ignore the value being matched.

A pattern matches a term if they have the same "shape" and if any atoms and literals in the pattern are identical to those in the term. Variables in the pattern will be bound to corresponding values in the term.

## Basic Examples

Here are some simple examples of pattern matching:

```erlang
B = 1.
2 = 2.
{ok, C} = {ok, 40}.
[H|T] = [1, 2, 3, 4].
```

- `B = 1.` matches because `B` is a variable and can be bound to the value `1`.
- `2 = 2.` matches because both sides are the same literal.
- `{ok, C} = {ok, 40}.` matches because the tuples have the same structure and the atom `ok` matches. `C` is bound to `40`.
- `[H|T] = [1, 2, 3, 4].` matches because the list `[1, 2, 3, 4]` can be split into head `H` (which is `1`) and tail `T` (which is `[2, 3, 4]`).

## Compound Data Structures

Pattern matching can be used with compound data structures such as tuples and lists:

```erlang
{ok, Value} = {ok, 100}. % Matches and binds Value to 100
[First, Second | Rest] = [a, b, c, d]. % Matches and binds First to 'a', Second to 'b', and Rest to '[c, d]'
```

## Pattern Matching Failures

Pattern matching fails if the patterns do not align with the terms. When this happens, an error is generated and the process exits. Here are some examples:

```erlang
1 = 2. % Fails because 1 is not equal to 2
{ok, A} = {failure, "Error message"}. % Fails because the atoms 'ok' and 'failure' do not match
[H|T] = []. % Fails because the list is empty and cannot be split into head and tail
```

## Using Pattern Matching in Functions

Pattern matching is extensively used to select which function clause to execute. Here’s an example:

```erlang
-module(math).
-export([factorial/1]).

factorial(0) ->
    1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).
```

In this example, the first clause matches when `N` is `0`, and the second clause matches for any positive integer `N`.

## Error Handling

When pattern matching fails, it generates an error that can be trapped and handled using `try...catch` or by monitoring processes. This allows you to build robust applications that can handle unexpected values gracefully.

### Example:

```erlang
-module(error_handling).
-export([safe_divide/2]).

safe_divide(A, B) ->
    try
        A / B
    catch
        error:badarith -> 
            {error, "Division by zero"}
    end.
```

In this example, attempting to divide by zero is caught and handled, returning a descriptive error tuple instead of crashing.

## Conclusion

Pattern matching in Erlang is a powerful feature that simplifies working with complex data structures and controlling the flow of your program. By understanding how to use pattern matching effectively, you can write more concise and readable code. Remember to handle pattern matching failures gracefully to build robust applications.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
