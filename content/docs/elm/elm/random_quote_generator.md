---
title: "Creating a Random Quotes App in Elm"
description: "Handling HTTP requests and JSON data in Elm."
icon: "code"
draft: false
---

In this tutorial, we will walk through building a simple Elm application that fetches random quotes from an API. We will break down the code into logical sections and explain each part in detail. By the end of this tutorial, you will have a good understanding of how to make HTTP requests and handle the responses in Elm.

## Introduction

Elm is a functional language for building reliable web applications. This tutorial will guide you through creating an application that fetches random quotes and displays them. You can find more information about handling JSON and HTTP in Elm in the [official guide](https://guide.elm-lang.org/effects/json.html).

## The Complete Code

Here's the full code for the application. We'll break it down into sections and explain each part.

```elm
-- Press a button to send a GET request for random quotes.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map4, field, int, string)
```

### Main Function

The `main` function is the entry point of our Elm application. It initializes the application with `Browser.element` and sets up the `init`, `update`, `subscriptions`, and `view` functions.

```elm
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
```

### Model

The `Model` represents the state of our application. We define a custom type `Model` with three possible states: `Failure`, `Loading`, and `Success` with a `Quote`.

```elm
type Model
  = Failure
  | Loading
  | Success Quote

type alias Quote =
  { quote : String
  , source : String
  , author : String
  , year : Int
  }
```

### Initialization

The `init` function initializes the application state. We start with the `Loading` state and immediately fetch a random quote.

```elm
init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getRandomQuote)
```

### Update

The `update` function handles messages and updates the model accordingly. We define two message types: `MorePlease` and `GotQuote`.

```elm
type Msg
  = MorePlease
  | GotQuote (Result Http.Error Quote)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getRandomQuote)

    GotQuote result ->
      case result of
        Ok quote ->
          (Success quote, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)
```

### Subscriptions

The `subscriptions` function manages subscriptions to external events. In this application, we don't have any subscriptions.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
```

### View

The `view` function renders the HTML based on the current model state. It uses a helper function `viewQuote` to display the quote or relevant messages.

```elm
view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Random Quotes" ]
    , viewQuote model
    ]

viewQuote : Model -> Html Msg
viewQuote model =
  case model of
    Failure ->
      div []
        [ text "I could not load a random quote for some reason. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success quote ->
      div []
        [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
        , blockquote [] [ text quote.quote ]
        , p [ style "text-align" "right" ]
            [ text "— "
            , cite [] [ text quote.source ]
            , text (" by " ++ quote.author ++ " (" ++ String.fromInt quote.year ++ ")")
            ]
        ]
```

### HTTP Request

The `getRandomQuote` function sends an HTTP GET request to fetch a random quote. It uses a decoder to parse the JSON response into a `Quote`.

```elm
getRandomQuote : Cmd Msg
getRandomQuote =
  Http.get
    { url = "https://elm-lang.org/api/random-quotes"
    , expect = Http.expectJson GotQuote quoteDecoder
    }

quoteDecoder : Decoder Quote
quoteDecoder =
  map4 Quote
    (field "quote" string)
    (field "source" string)
    (field "author" string)
    (field "year" int)
```

## Conclusion

This tutorial covered the essential parts of an Elm application that fetches and displays random quotes. You learned about the main structure of an Elm app, how to define a model, handle updates, manage subscriptions, and make HTTP requests.

Feel free to modify the code and experiment with different features to deepen your understanding of Elm. Happy coding!