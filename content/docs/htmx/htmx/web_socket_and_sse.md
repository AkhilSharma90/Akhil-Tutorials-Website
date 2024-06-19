---
title: "Web Socket and SSE"
description: "htmx is a library that allows you to access modern browser features directly from HTML, rather than using javascript."
icon: "/"
draft: false
---

htmx has experimental support for declarative use of both WebSockets and Server Sent Events.

Note: In htmx 2.0, these features will be migrated to extensions. These new extensions are already available in htmx 1.7+ and, if you are writing new code, you are encouraged to use the extensions instead. All new feature work for both SSE and web sockets will be done in the extensions.

## WebSockets
If you wish to establish a WebSocket connection in htmx, you use the hx-ws attribute:
```html
<div hx-ws="connect:wss:/chatroom">
    <div id="chat_room">
        ...
    </div>
    <form hx-ws="send:submit">
        <input name="chat_message">
    </form>
</div>
```

The connect declaration established the connection, and the send declaration tells the form to submit values to the socket on submit.

More details can be found on the hx-ws attribute page

## Server Sent Events
Server Sent Events are a way for servers to send events to browsers. It provides a higher-level mechanism for communication between the server and the browser than websockets.

If you want an element to respond to a Server Sent Event via htmx, you need to do two things:

Define an SSE source. To do this, add a hx-sse attribute on a parent element with a connect:<url> declaration that specifies the URL from which Server Sent Events will be received.

Define elements that are descendents of this element that are triggered by server sent events using the hx-trigger="sse:<event_name>" syntax

Here is an example:
```html
<body hx-sse="connect:/news_updates">
    <div hx-trigger="sse:new_news" hx-get="/news"></div>
</body>
```
Depending on your implementation, this may be more efficient than the polling example above since the server would notify the div if there was new news to get, rather than the steady requests that a poll causes.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
