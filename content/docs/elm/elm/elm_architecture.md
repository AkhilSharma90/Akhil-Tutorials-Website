---
title: "3. Elm Architecture"
description: "Understanding The Elm Architecture for building robust web applications."
icon: "code"
draft: false
---

## Elm Architecture

Understanding The Elm Architecture (TEA) is crucial for building applications in Elm. TEA is a pattern for architecting web applications. It is simple yet powerful, helping you to build well-structured and easily maintainable applications.

### Components of The Elm Architecture

TEA revolves around three main concepts: Model, Update, and View. Together, these components create a unidirectional data flow that is easy to understand and debug.

#### Model

The model represents the state of your application. It's a data structure (like a record) that contains all the information needed to render your app.

```elm
type alias Model = 
    { message : String 
    }
```

#### Update

The update function is where you define how your application responds to different messages (or actions). It takes a message and the current model, and returns an updated model.

```elm
type Msg = UpdateMessage String

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateMessage newMessage ->
            { model | message = newMessage }
```

#### View

The view function takes the current model and returns HTML. Elm uses a virtual DOM, which makes updating the view very efficient.

```elm
view : Model -> Html Msg
view model =
    Html.text model.message
```

### Simple Application Example

Let's put these concepts together in a simple Elm application. The application will display a message and update it when a button is clicked.

```elm
module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- MODEL
type alias Model = 
    { message : String 
    }

initModel : Model
initModel = 
    { message = "Hello, Elm!" 
    }

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
    Html.beginnerProgram 
        { model = initModel
        , view = view
        , update = update 
        }
```

In this program, clicking the button triggers an `UpdateMessage`, which updates the model's message. The view then reflects this change.

### Advanced Topics in TEA

As you become more familiar with TEA, you can explore its advanced features, which include handling side effects, managing complex state, and integrating with JavaScript.

#### Handling Side Effects

In Elm, side effects (like HTTP requests) are managed in a controlled way using commands (`Cmd`). The Elm Architecture handles these commands to perform side effects and then routes the results back to your application.

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

#### Managing Complex State

For more complex applications, you can split your model and update logic into smaller parts. This approach helps in managing state more effectively and keeps your codebase organized.

#### Integrating with JavaScript

Elm allows you to integrate with JavaScript using ports. Ports enable communication between Elm and JavaScript, allowing you to leverage JavaScript libraries and functionality within your Elm application.

```elm
port module Main exposing (..)

port alert : String -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AlertMessage ->
            (model, alert "This is an alert!")
```

Understanding and mastering The Elm Architecture will enable you to build robust, maintainable, and scalable web applications in Elm.