---
title: "5. Create a Simple Registration Form"
description: "Learn how to create interactive forms and handle user input in Elm."
icon: "form"
draft: false
---

In this tutorial, we will walk through building a basic user registration form using Elm. This form will allow users to input their name and password, and it will validate that the password and password confirmation match. By following this tutorial, you'll learn how to handle form inputs and state updates in Elm.

## Introduction

Elm is a functional language designed for building robust web applications. This tutorial will guide you through creating a simple registration form, demonstrating how to handle user input and perform validation. For more detailed information on handling forms in Elm, refer to the [official guide](https://guide.elm-lang.org/architecture/forms.html).

## The Complete Code

Here is the complete code for the registration form application. We will break it down into sections and explain each part.

```elm
-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
```

### Main Function

The `main` function initializes the Elm application using `Browser.sandbox`. It sets up the `init`, `update`, and `view` functions.

```elm
main =
  Browser.sandbox { init = init, update = update, view = view }
```

### Model

The `Model` represents the state of our application. It includes three fields: `name`, `password`, and `passwordAgain`.

```elm
type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }

init : Model
init =
  Model "" "" ""
```

### Update

The `update` function handles messages that update the model. We define three message types: `Name`, `Password`, and `PasswordAgain`.

```elm
type Msg
  = Name String
  | Password String
  | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }
```

### View

The `view` function renders the HTML based on the current model state. It uses helper functions `viewInput` to create input fields and `viewValidation` to display the validation message.

```elm
view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
```

## Explanation

### Main Function

The `main` function initializes our Elm application in a sandbox environment, which is suitable for simple applications that do not require advanced features like HTTP requests or subscriptions. It specifies the `init`, `update`, and `view` functions to manage the application state and render the UI.

```elm
main =
  Browser.sandbox { init = init, update = update, view = view }
```

### Model

The `Model` type alias defines the structure of our application state. It includes three fields: `name`, `password`, and `passwordAgain`, all of which are strings. The `init` function initializes these fields to empty strings.

```elm
type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }

init : Model
init =
  Model "" "" ""
```

### Update

The `update` function takes a message and the current model, and returns an updated model. We define three message types: `Name`, `Password`, and `PasswordAgain`, each carrying a string payload. Depending on the message received, the function updates the corresponding field in the model.

```elm
type Msg
  = Name String
  | Password String
  | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }
```

### View

The `view` function generates the HTML for our application. It uses the `viewInput` helper function to create input fields for the name, password, and password confirmation. It also uses the `viewValidation` function to display a validation message based on whether the passwords match.

```elm
view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
```

### Helper Functions

The `viewInput` function creates an input field with the specified type, placeholder, value, and message handler. The `viewValidation` function displays a validation message based on whether the password and password confirmation match.

```elm
viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
```

## Conclusion

This tutorial covered the essential parts of an Elm application for a simple user registration form. You learned how to handle user input, update the model, and perform validation. This example provides a solid foundation for creating more complex forms and applications in Elm.
