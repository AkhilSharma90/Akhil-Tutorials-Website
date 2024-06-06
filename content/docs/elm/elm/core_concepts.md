---
title: "Core Concepts in Elm"
description: "Understanding the foundational concepts that make Elm robust and functional."
icon: "code"
draft: false
---

Elm is built on a set of core concepts that make it robust and functional. Understanding these concepts is key to becoming proficient in Elm. Let's explore some of these fundamental ideas.

## Types and Type Annotations

Elm is a statically typed language, meaning the type of every variable and expression is known at compile time. This feature makes your code more reliable and easier to maintain.

### Basic Types

Elm has several basic types such as `Int`, `Float`, `String`, `Bool`, and more.

```elm
myInt : Int
myInt = 5

myString : String
myString = "Hello, Elm!"
```

### Type Annotations

Type annotations are used to explicitly declare the type of a function or variable. While not mandatory, they are a good practice.

```elm
add : Int -> Int -> Int
add x y = x + y
```
This function, `add`, takes two `Int` values and returns an `Int`.

## Functions and Function Composition

Functions are the building blocks of Elm programs. Elm functions are pure, meaning they always produce the same output for the same input and have no side effects.

### Defining Functions

Functions are defined with a name, a list of parameters, an equals sign, and the function body.

```elm
greet : String -> String
greet name = "Hello, " ++ name
```

### Function Composition

Function composition is a way to combine simple functions to build more complex ones. Elm uses the `>>` and `<<` operators for composition.

```elm
uppercase : String -> String
uppercase str = String.toUpper str

exclaim : String -> String
exclaim str = str ++ "!"

excitedGreet : String -> String
excitedGreet = greet >> uppercase >> exclaim
-- excitedGreet "Elm" returns "HELLO, ELM!"
```

## Records and Modules

Records in Elm are similar to objects in JavaScript. They are used to store structured data.

### Records

```elm
type alias Person = 
    { name : String
    , age : Int 
    }

bob : Person
bob = { name = "Bob", age = 42 }
```

### Modules

Elm uses modules to organize code. Modules can contain functions, type aliases, and type definitions. You can expose certain parts of a module to be used in other modules.

```elm
module Math exposing (add, subtract)

add : Int -> Int -> Int
add a b = a + b

subtract : Int -> Int -> Int
subtract a b = a - b
```

In this Math module, `add` and `subtract` functions are exposed.

## Advanced Elm

As you get more comfortable with the basics of Elm, you can start exploring its advanced features. These include handling side effects, making HTTP requests, and decoding JSON. These concepts are essential for building complex and interactive web applications.

### Handling Side Effects

In Elm, side effects (like HTTP requests) are managed in a controlled way using `Cmd`. The Elm Architecture handles these commands to perform side effects and then routes the results back to your application.

### The Cmd Type

`Cmd` is a type that represents a side effect that needs to be performed. It's used in conjunction with the `update` function to handle asynchronous actions.

```elm
type Msg = FetchData | ReceiveData String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FetchData ->
            (model, fetchDataCmd)

        ReceiveData data ->
            ({ model | data = data }, Cmd.none)
```

In this example, `FetchData` triggers an HTTP request, and `ReceiveData` updates the model with the received data.
