---
title: "4. Working with HTTP Requests"
description: "Handling HTTP requests and JSON data in Elm."
icon: "code"
draft: false
---

## Working with HTTP Requests

Elm provides a smooth way to handle HTTP requests with the `Http` module.

### Making a GET Request

Here's an example of how to make a GET request to fetch data:

```elm
fetchDataCmd : Cmd Msg
fetchDataCmd =
    Http.get
        { url = "https://api.example.com/data"
        , expect = Http.expectString ReceiveData
        }
```

This function creates a command that, when executed, sends a GET request to the specified URL and expects a string response.

### Handling the Response

To handle the response from the GET request, you need to define a message and update function that processes the response:

```elm
type Msg
    = ReceiveData (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveData (Ok data) ->
            ({ model | data = data }, Cmd.none)

        ReceiveData (Err error) ->
            ({ model | error = Just error }, Cmd.none)
```

### JSON Decoding

Elm handles JSON data using decoders. A JSON decoder describes how to convert JSON data into Elm values.

### Decoding JSON

Here's an example of a JSON decoder:

```elm
type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
```

This `userDecoder` can be used to decode a JSON object into a `User` record.

### Using JSON Decoder in HTTP Request

You can use this decoder with the `Http` module to decode the response of an HTTP request:

```elm
fetchUserCmd : Cmd Msg
fetchUserCmd =
    Http.get
        { url = "https://api.example.com/user"
        , expect = Http.expectJson ReceiveUser userDecoder
        }
```

In this example, when the data is received from the URL, it's decoded using `userDecoder`.

### Handling JSON Response

To handle the JSON response, you need to define a message and update function that processes the decoded data:

```elm
type Msg
    = ReceiveUser (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveUser (Ok user) ->
            ({ model | user = Just user }, Cmd.none)

        ReceiveUser (Err error) ->
            ({ model | error = Just error }, Cmd.none)
```

### Complete Example

Combining all the pieces, here's a complete example of fetching and decoding JSON data in Elm:

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, int, string, map2)

-- MODEL

type alias Model =
    { user : Maybe User
    , error : Maybe Http.Error
    }

initModel : Model
initModel =
    { user = Nothing
    , error = Nothing
    }

type alias User =
    { id : Int
    , name : String
    }

-- UPDATE

type Msg
    = FetchUser
    | ReceiveUser (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FetchUser ->
            (model, fetchUserCmd)

        ReceiveUser (Ok user) ->
            ({ model | user = Just user, error = Nothing }, Cmd.none)

        ReceiveUser (Err error) ->
            ({ model | error = Just error, user = Nothing }, Cmd.none)

fetchUserCmd : Cmd Msg
fetchUserCmd =
    Http.get
        { url = "https://api.example.com/user"
        , expect = Http.expectJson ReceiveUser userDecoder
        }

userDecoder : Decoder User
userDecoder =
    map2 User
        (field "id" int)
        (field "name" string)

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ case model.user of
            Just user ->
                div [] [ text ("User: " ++ user.name ++ ", ID: " ++ String.fromInt user.id) ]

            Nothing ->
                text "No user data"

        , case model.error of
            Just error ->
                div [] [ text ("Error: " ++ Http.errorToString error) ]

            Nothing ->
                text ""
        
        , button [ onClick FetchUser ] [ text "Fetch User" ]
        ]

-- MAIN

main =
    Browser.sandbox { init = initModel, update = update, view = view }
```

In this complete example, clicking the "Fetch User" button triggers an HTTP GET request to fetch user data, decodes the JSON response, and updates the view accordingly.