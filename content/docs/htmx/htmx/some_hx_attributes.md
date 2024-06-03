---
title: "Learning More Abuot HTMX"
description: "Learn about what HTMX is and how you can use it."
icon: "code"
draft: false
---

## Other hx-Attributes

### Indicators

You can specify an element that should be shown or hidden when the request is in flight using the hx-indicator attribute.

Here is a div that hides itself and shows a spinner when it is loading:

```html
<div hx-get="/slow" hx-trigger="click" hx-target="#parent-div">
    <span hx-indicator="#spinner">Load the slow content</span>
</div>
<img id="spinner" src="/img/spinner.gif" style="display:none;">
```

Indicators are shown when a request is initiated, and hidden when the request is finished.

### Selecting Content to Swap

You can select a portion of the response to use for the swap, instead of using the entire response, using the hx-select attribute. The value of this attribute is a CSS selector that will be run against the response. If the selector matches more than one element, they will all be swapped into the target. Here is a div that will use only the #sub-content from the response, rather than the entire response:

```html
<div hx-get="/slow" hx-select="#sub-content" hx-trigger="click" hx-target="#parent-div">
    Click Me!
</div>
```

Here is an example of HTML you might get back from /slow:

```html
<!doctype html>
<html>
    <head>
        <title>My Title</title>
    </head>
    <body>
        <div id="sub-content">This content was slow!</div>
    </body>
</html>
```

### URL Variables

Sometimes you want to refer to the value of an input in a URL. You can do this with the hx-vals attribute. The value of the attribute should be a JSON object with keys that are the names of the variables you want to insert into the URL. Here is a div that will use the value of an input with the name of “page” in the URL:

```html
<div hx-get="/search?page={page}" hx-vals='{"page": "input[name=page]"}'>
    Click Me!
</div>
<input type="hidden" name="page" value="2">
```

### Pushing States

htmx supports the HTML5 History API via the hx-push-url attribute. When this attribute is present, the URL of the page will be updated whenever a request is made. Here is a div that will update the URL to /foo when clicked:

```html
<div hx-get="/foo" hx-push-url="true">
    Click Me!
</div>
```

When clicked, the div will load the content from /foo into the div, and the URL will be updated to /foo. This allows you to easily create single-page applications with htmx.

### Handling Request & Response Headers

htmx supports the hx-headers attribute, which allows you to set custom headers on the request. Here is a div that will set the X-My-Header header to “My Value”:

```html
<div hx-get="/foo" hx-headers='{"X-My-Header": "My Value"}'>
    Click Me!
</div>
```

htmx also supports the hx-trigger-headers attribute, which allows you to set custom headers on the request based on the triggering event. Here is a div that will set the X-My-Header header to “My Value” when clicked:

```html
<div hx-get="/foo" hx-trigger-headers='{"click": {"X-My-Header": "My Value"}}'>
    Click Me!
</div>
```

### Other Headers

htmx supports a number of other headers that can be used to customize the behavior of the request and response.

- **HX-Trigger** - This header can be used to trigger client side events when a response is received. Here is a response that will trigger the foo event on the body:

```html
HX-Trigger: foo
```

- **HX-Trigger-After-Settle** - This header can be used to trigger client side events when the request is settled (e.g. after all swaps have been completed). Here is a response that will trigger the foo event on the body after the request is settled:

```html
HX-Trigger-After-Settle: foo
```

- **HX-Trigger-After-Swap** - This header can be used to trigger client side events when the swap is completed. Here is a response that will trigger the foo event on the body after the swap is completed:

```html
HX-Trigger-After-Swap: foo
```

- **HX-Push** - This header can be used to push a new URL to the history stack. Here is a response that will push the URL /foo to the history stack:

```html
HX-Push: /foo
```

- **HX-Replace-Url** - This header can be used to replace the current URL in the history stack. Here is a response that will replace the current URL with /foo:

```html
HX-Replace-Url: /foo
```

- **HX-Redirect** - This header can be used to redirect the browser to a new URL. Here is a response that will redirect the browser to /foo:

```html
HX-Redirect: /foo
```

### Polling

htmx supports polling via the hx-trigger attribute. Here is a div that will poll the /clock endpoint every second:

```html
<div hx-get="/clock" hx-trigger="every 1s">
    Loading...
</div>
```

Polling can be stopped by removing the hx-trigger attribute. Here is a button that will stop the polling:

```html
<button hx-swap-oob="true" hx-trigger="click" hx-get="/stop_polling">
    Stop Polling
</button>
```

Here is the /stop_polling endpoint that will stop the polling:

```html
<div hx-trigger="none">
    Polling Stopped
</div>
```

### Progress Indicator

htmx supports progress indicators via the hx-indicator attribute. Here is a div that will show a spinner while the request is in flight:

```html
<div hx-get="/slow" hx-indicator="#spinner">
    Click Me!
</div>
<img id="spinner" src="/img/spinner.gif" style="display:none;">
```

The indicator will be shown when the request is initiated, and hidden when the request is finished.

### WebSockets

htmx supports WebSockets via the hx-ws attribute. Here is a div that will connect to a WebSocket and send a message when clicked:

```html
<div hx-ws="connect:/ws">
    <div hx-swap-oob="true">
        <span hx-ws-send="click:hello">Click Me!</span>
    </div>
</div>
```

The hx-ws attribute can also be used to receive messages from a WebSocket. Here is a div that will receive messages from a WebSocket and update itself:

```html
<div hx-ws="connect:/ws receive:update">
    Waiting for update...
</div>
```

### SSE

htmx supports Server-Sent Events via the hx-sse attribute. Here is a div that will connect to an SSE endpoint and update itself with the received message:

```html
<div hx-sse="connect:/sse">
    Waiting for update...
</div>
```

The hx-sse attribute can also be used to send messages to an SSE endpoint. Here is a div that will send a message to an SSE endpoint when clicked:

```html
<div hx-sse="connect:/sse send:hello">
    Click Me!
</div>
```

### Client Side Events

htmx supports client side events via the hx-on attribute. Here is a div that will trigger the foo event on the body when clicked:

```html
<div hx-on="click:foo">
    Click Me!
</div>
```

The hx-on attribute can also be used to listen for events. Here is a div that will listen for the foo event and update itself:

```html
<div hx-on="foo:update">
    Waiting for foo...
</div>
```