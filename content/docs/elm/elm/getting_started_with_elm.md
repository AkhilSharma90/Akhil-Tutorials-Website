---
title: "Getting Started With Elm"
description: "Elm Lang description"
icon: "code"
draft: false
---

## Introduction to Elm

Elm is a delightful language for reliable web applications. It compiles to JavaScript, but it's much
more than just a language; it's a framework for making web development more robust and
pleasant. One of the most striking features of Elm is its emphasis on simplicity and quality
tooling, making it an excellent choice for both beginners and experienced developers.

## Why Elm?

**Simplicity and Safety**

Elm's syntax is clean and easy to understand, making it an ideal starting point for those new to
programming. It avoids runtime errors in your application, thanks to its strong type system and
compiler checks. This means fewer crashes and unexpected behavior in your applications.

**Performance and Maintainability**
Elm applications are known for their performance. The language is designed for easy
refactoring and maintainability, so your codebase remains manageable, even as it grows in
complexity.
**Great Developer Experience**

Elm provides a friendly compiler that not only catches errors but also suggests how to fix them.
This feature, along with a strong set of tools and a helpful community, makes learning and
developing with Elm a rewarding experience.
In the following sections, we will delve into Elm's setup, basic syntax, core concepts, and some
advanced features, all accompanied by practical code examples. This journey will equip you
with the foundational knowledge and skills to start building your own Elm applications.

## Getting Started with Elm

Elm provides a smooth entry point for beginners, and setting it up is straightforward. Here's how
you can get started with Elm and write your first Elm program.

**Installation and Setup**

1. Install Elm: Visit Elm's official website and follow the instructions to install Elm for your
   operating system.2. Editor Setup: For a better experience, use an editor with Elm support, like Visual Studio
   Code. Install the Elm language support extensions for syntax highlighting and
   auto-completion.
2. Create Your First Elm File: Create a new file with the extension .elm. For instance,
   HelloWorld.elm.

## Basic Syntax and Structure

Elm is a purely functional language, and its syntax reflects this. Here's a quick overview:
● Comments: Single-line comments start with --, and multi-line comments are enclosed in
{- and -}.
● Functions: Functions are central in Elm. A simple function to add two numbers looks like
this:

```elm
add a b = a + b
```

●
●
Variables: Elm uses immutable variables. Once a variable is declared, its value can't
change.
Types: Types are explicit in Elm, ensuring code reliability.

## "Hello, World!" in Elm

Let's write the classic "Hello, World!" program. Create a file named HelloWorld.elm and write the
following code:

```elm
module HelloWorld exposing (..)
import Html exposing (text)
main =
text "Hello, World!"
```

This program uses the Html module to display text. The main function is the entry point of every
Elm program. Here, it's using text from the Html module to render "Hello, World!" on the screen.
To run this program:
​ Open your terminal or command prompt.
​ Navigate to the directory containing your HelloWorld.elm file.
​ Run elm reactor.
​ Open your web browser and go to http://localhost:8000.
​ Click on HelloWorld.elm to see your program running.

## Elm Architecture

Understanding The Elm Architecture (TEA) is crucial for building applications in Elm. TEA is a
pattern for architecting web applications. It is simple yet powerful, helping you to build
well-structured and easily maintainable applications.

### Components of The Elm Architecture

TEA revolves around three main concepts: Model, Update, and View. Together, these
components create a unidirectional data flow that is easy to understand and debug.
**Model**

The model represents the state of your application. It's a data structure (like a record) that
contains all the information needed to render your app.

```elm
type alias Model = { message : String }
```

**Update**

The update function is where you define how your application responds to different messages
(or actions). It takes a message and the current model, and returns an updated model.

```elm
type Msg = UpdateMessage String
update : Msg -> Model -> Model
update msg model =
case msg of
UpdateMessage newMessage ->
{ model | message = newMessage }
```

**View**
The view function takes the current model and returns HTML. Elm uses a virtual DOM, which
makes updating the view very efficient.

```elm
view : Model -> Html Msg
view model =
Html.text model.messageSimple Application Example
```

**Simple Application Example**

Let's put these concepts together in a simple Elm application. The application will display a
message and update it when a button is clicked.

```elm
module Main exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
-- MODEL
type alias Model = { message : String }
initModel : Model
initModel = { message = "Hello, Elm!" }
-- UPDATE
type Msg = UpdateMessage
update : Msg -> Model -> Model
update msg model =
case msg of
UpdateMessage ->
{ model | message = "Button clicked!" }
-- VIEW
view : Model -> Html Msg
view model =
div []
[ text model.message
, button [ onClick UpdateMessage ] [ text "Click me!" ]
]
-- MAIN
main =
Html.beginnerProgram { model = initModel, view = view, update = update }
```

In this program, clicking the button triggers an UpdateMessage, which updates the model's
message. The view then reflects this change.

## Core Concepts in Elm

Elm is built on a set of core concepts that make it robust and functional. Understanding these
concepts is key to becoming proficient in Elm. Let's explore some of these fundamental ideas.
**Types and Type Annotations**

Elm is a statically typed language, meaning the type of every variable and expression is known
at compile time. This feature makes your code more reliable and easier to maintain.

**Basic Types**

Elm has several basic types like Int, Float, String, Bool, and more.

```elm
myInt : Int
myInt = 5
myString : String
myString = "Hello, Elm!"
```

**Type Annotations**
Type annotations are used to explicitly declare the type of a function or variable. They are not
mandatory but are a good practice.

```elm 
add : Int -> Int -> Int
add x y = x + y
```
This function, add, takes two Int values and returns an Int.

**Functions and Function Composition**

Functions are the building blocks of Elm programs. Elm functions are pure, meaning they
always produce the same output for the same input and have no side effects.

**Defining Functions**

Functions are defined with a name, a list of parameters, an equals sign, and the function body.
```elm
greet : String -> String
greet name = "Hello, " ++ name
```

**Function Composition**

Function composition is a way to combine simple functions to build more complex ones. Elm
uses the >> and << operators for composition.
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
**Records**
```elm
type alias Person = {
name : String,
age : Int
}
bob : Person
bob = { name = "Bob", age = 42 }
```

**Modules**
Elm uses modules to organize code. Modules can contain functions, type aliases, and type
definitions. You can expose certain parts of a module to be used in other modules.
```elm
module Math exposing (add, subtract)add : Int -> Int -> Int
add a b = a + b
subtract : Int -> Int -> Int
subtract a b = a - b
```

In this Math module, add and subtract functions are exposed.

## Advanced Elm
As you get more comfortable with the basics of Elm, you can start exploring its advanced
features. These include handling side effects, making HTTP requests, and decoding JSON.
These concepts are essential for building complex and interactive web applications.

**Handling Side Effects**
In Elm, side effects (like HTTP requests) are managed in a controlled way using Cmd. The Elm
Architecture handles these commands to perform side effects and then routes the results back
to your application.

**The Cmd Type**

Cmd is a type that represents a side effect that needs to be performed. It's used in conjunction
with the update function to handle asynchronous actions.
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

In this example, FetchData triggers an HTTP request, and ReceiveData updates the model with
the received data.

## Working with HTTP Requests
Elm provides a smooth way to handle HTTP requests with the Http module.

**Making a GET Request**

Here's an example of how to make a GET request to fetch data:
```elm
fetchDataCmd : Cmd Msg
fetchDataCmd =
Http.get
{ url = "https://api.example.com/data"
, expect = Http.expectString ReceiveData
}
```

This function creates a command that, when executed, sends a GET request to the specified
URL and expects a string response.

## JSON Decoding
Elm handles JSON data using decoders. A JSON decoder describes how to convert JSON data
into Elm values.

**Decoding JSON**
Here's an example of a JSON decoder:

```elm
type alias User = {
id : Int,
name : String
}
userDecoder : Decoder User
userDecoder =
Decode.map2 User
(Decode.field "id" Decode.int)
(Decode.field "name" Decode.string)
```

This userDecoder can be used to decode a JSON object into a User record.


## Using JSON Decoder in HTTP Request
You can use this decoder with the Http module to decode the response of an HTTP request:
```elm
fetchUserCmd : Cmd Msg
fetchUserCmd =
Http.get{ url = "https://api.example.com/user"
, expect = Http.expectJson ReceiveData userDecoder
}```

In this example, when the data is received from the URL, it's decoded using userDecoder.
````
