---
title: "5. Building Forms and Handling User Input"
description: "Learn how to create interactive forms and handle user input in Elm."
icon: "form"
draft: false
---

## Building Forms and Handling User Input

Elm makes it straightforward to build forms and handle user input, ensuring a smooth and type-safe experience.

### Creating a Simple Form

Let's start with a simple form that collects a user's name and email address.

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, div, text, input, button, form)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)

-- MODEL

type alias Model =
    { name : String
    , email : String
    }

initModel : Model
initModel =
    { name = ""
    , email = ""
    }

-- UPDATE

type Msg
    = UpdateName String
    | UpdateEmail String
    | Submit

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateName name ->
            { model | name = name }

        UpdateEmail email ->
            { model | email = email }

        Submit ->
            model

-- VIEW

view : Model -> Html Msg
view model =
    form [ onSubmit (always Submit) ]
        [ div []
            [ text "Name: "
            , input [ type_ "text", value model.name, onInput UpdateName ] []
            ]
        , div []
            [ text "Email: "
            , input [ type_ "email", value model.email, onInput UpdateEmail ] []
            ]
        , button [ type_ "submit" ] [ text "Submit" ]
        ]

-- MAIN

main =
    Browser.sandbox { init = initModel, update = update, view = view }
```

### Handling Form Submission

When the form is submitted, we can process the collected data. Here, we will just print the data to the console for simplicity.

```elm
-- UPDATE

type Msg
    = UpdateName String
    | UpdateEmail String
    | Submit
    | FormSubmitted

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateName name ->
            ({ model | name = name }, Cmd.none)

        UpdateEmail email ->
            ({ model | email = email }, Cmd.none)

        Submit ->
            (model, submitForm model)

        FormSubmitted ->
            (model, Cmd.none)

submitForm : Model -> Cmd Msg
submitForm model =
    let
        name = model.name
        email = model.email
    in
    Debug.log "Form Submitted" (name, email) |> always FormSubmitted
```

### Adding Validation

Form validation is crucial to ensure the user inputs correct data. Here is how you can add basic validation:

```elm
-- MODEL

type alias Model =
    { name : String
    , email : String
    , errors : List String
    }

initModel : Model
initModel =
    { name = ""
    , email = ""
    , errors = []
    }

-- UPDATE

type Msg
    = UpdateName String
    | UpdateEmail String
    | Submit
    | FormSubmitted

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateName name ->
            ({ model | name = name }, Cmd.none)

        UpdateEmail email ->
            ({ model | email = email }, Cmd.none)

        Submit ->
            if validateForm model then
                (model, submitForm model)
            else
                ({ model | errors = ["Invalid input"] }, Cmd.none)

        FormSubmitted ->
            (model, Cmd.none)

validateForm : Model -> Bool
validateForm model =
    String.length model.name > 0 && String.contains "@" model.email

submitForm : Model -> Cmd Msg
submitForm model =
    let
        name = model.name
        email = model.email
    in
    Debug.log "Form Submitted" (name, email) |> always FormSubmitted
```

### Displaying Validation Errors

Update the view to display validation errors:

```elm
-- VIEW

view : Model -> Html Msg
view model =
    form [ onSubmit (always Submit) ]
        [ div []
            [ text "Name: "
            , input [ type_ "text", value model.name, onInput UpdateName ] []
            ]
        , div []
            [ text "Email: "
            , input [ type_ "email", value model.email, onInput UpdateEmail ] []
            ]
        , div []
            (List.map (div [] << text) model.errors)
        , button [ type_ "submit" ] [ text "Submit" ]
        ]
```

In this topic, we covered creating a simple form, handling form submission, adding validation, and displaying validation errors. These fundamentals will help you build more complex and interactive forms in Elm applications.