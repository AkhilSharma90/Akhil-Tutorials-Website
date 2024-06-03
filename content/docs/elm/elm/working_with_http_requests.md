---
title: "Working with HTTP Requests"
description: "Elm Lang description"
icon: "code"
draft: false
---


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
