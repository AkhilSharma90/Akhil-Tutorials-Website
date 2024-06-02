---
title: "Learning More Abuot HTMX"
description: "Learn about what HTMX is and how you can use it."
icon: "code"
draft: false
---

## htmx in a Nutshell

htmx is a library that allows you to access modern browser features directly from HTML, rather than using javascript. To understand htmx, first let’s take a look at an anchor tag:

```html
<a href="/blog">Blog</a>
```

This anchor tag tells a browser:  
“When a user clicks on this link, issue an HTTP GET request to ‘/blog’ and load the response content into the browser window”.

With that in mind, consider the following bit of HTML:

```html
<button hx-post="/clicked"
        hx-trigger="click"
        hx-target="#parent-div"
        hx-swap="outerHTML">
    Click Me!
</button>
```

This tells htmx:  
“When a user clicks on this button, issue an HTTP POST request to ‘/clicked’ and use the content from the response to replace the element with the id parent-div in the DOM”.

htmx extends and generalizes the core idea of HTML as a hypertext, opening up many more possibilities directly within the language:

- Now any element, not just anchors and forms, can issue an HTTP request
- Now any event, not just clicks or form submissions, can trigger requests
- Now any HTTP verb, not just GET and POST, can be used
- Now any element, not just the entire window, can be the target for update by the request

Note that when you are using htmx, on the server side you typically respond with HTML, not JSON. This keeps you firmly within the original web programming model, using Hypertext As The Engine Of Application State without even needing to really understand that concept.

It’s worth mentioning that, if you prefer, you can use the data- prefix when using htmx:

```html
<a data-hx-post="/click">Click Me!</a>
```

## Installing

Htmx is a dependency-free, browser-oriented javascript library. This means that using it is as simple as adding a `<script>` tag to your document head. No need for complicated build steps or systems.

### Via A CDN (e.g. unpkg.com)

The fastest way to get going with htmx is to load it via a CDN. You can simply add this to your head tag and get going:

```html
<script src="https://unpkg.com/htmx.org@1.9.10"
        integrity="sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC" crossorigin="anonymous"></script>
```

### Download a copy

The next easiest way to install htmx is to simply copy it into your project. Download htmx.min.js from unpkg.com and add it to the appropriate directory in your project and include it where necessary with a `<script>` tag:

```html
<script src="/path/to/htmx.min.js"></script>
```

You can also add extensions this way, by downloading them from the ext/ directory.

### npm

For npm-style build systems, you can install htmx via npm:

```sh
npm install htmx.org
```

After installing, you’ll need to use appropriate tooling to use node_modules/htmx.org/dist/htmx.js (or .min.js). For example, you might bundle htmx with some extensions and project-specific code.

### Webpack

If you are using webpack to manage your javascript:

- Install htmx via your favourite package manager (like npm or yarn)
- Add the import to your index.js

```js
import 'htmx.org';
```

If you want to use the global htmx variable (recommended), you need to inject it to the window scope:

- Create a custom JS file
- Import this file to your index.js (below the import from step 2)

```js
import 'path/to/my_custom.js';
```

Then add this code to the file:

```js
window.htmx = require('htmx.org');
```

Finally, rebuild your bundle.

## AJAX

The core of htmx is a set of attributes that allow you to issue AJAX requests directly from HTML:

| Attribute | Description                            |
|-----------|----------------------------------------|
| hx-get    | Issues a GET request to the given URL  |
| hx-post   | Issues a POST request to the given URL |
| hx-put    | Issues a PUT request to the given URL  |
| hx-patch  | Issues a PATCH request to the given URL|
| hx-delete | Issues a DELETE request to the given URL|

Each of these attributes takes a URL to issue an AJAX request to. The element will issue a request of the specified type to the given URL when the element is triggered:

```html
<div hx-put="/messages">
    Put To Messages
</div>
```

This tells the browser:  
When a user clicks on this div, issue a PUT request to the URL /messages and load the response into the div.

### Triggering Requests

By default, AJAX requests are triggered by the “natural” event of an element:

- input, textarea & select are triggered on the change event
- form is triggered on the submit event
- everything else is triggered by the click event

If you want different behavior you can use the hx-trigger attribute to specify which event will cause the request.

Here is a div that posts to /mouse_entered when a mouse enters it:

```html
<div hx-post="/mouse_entered" hx-trigger="mouseenter">
    [Here Mouse, Mouse!]
</div>
```

### Trigger Modifiers

A trigger can also have a few additional modifiers that change its behavior. For example, if you want a request to only happen once, you can use the once modifier for the trigger:

```html
<div hx-post="/mouse_entered" hx-trigger="mouseenter once">
    [Here Mouse, Mouse!]
</div>
```

Other modifiers you can use for triggers are:

- changed - only issue a request if the value of the element has changed
- delay:<time interval> - wait the given amount of time (e.g. 1s) before issuing the request. If the event triggers again, the countdown is reset.
- throttle:<time interval> - wait the given amount of time (e.g. 1s) before issuing the request. Unlike delay if a new event occurs before the time limit is hit the event will be discarded, so the request will trigger at the end of the time period.
- from:<CSS Selector> - listen for the event on a different element. This can be used for things like keyboard shortcuts.

You can use these attributes to implement many common UX patterns, such as Active Search:

```html
<input type="text" name="q"
       hx-get="/trigger_delay"
       hx-trigger="keyup changed delay:500ms"
       hx-target="#search-results"
       placeholder="Search...">
<div id="search-results"></div>
```

This input will issue a request 500 milliseconds after a key up event if the input has been changed and inserts the results into the div with the id search-results. Multiple triggers can be specified in the hx-trigger attribute, separated by commas.

### Click to Edit

The click to edit pattern provides a way to offer inline editing of all or part of a record without a page refresh.

- This pattern starts with a UI that shows the details of a contact. The div has a button that will get the editing UI for the contact from /contact/1/edit

```html
<div hx-target="this" hx-swap="outerHTML">
    <div>
        <label>First Name</label>: Joe
    </div>
    <div>
        <label>Last Name</label>: Blow
    </div>
    <div>
        <label>Email</label>: joe@blow.com
    </div>
    <button hx-get="/contact/1/edit" class="btn btn-primary">
        Click To Edit
    </button>
</div>
```

This returns a form that can be used to edit the contact

```html
<form hx-put="/contact/1" hx-target="this" hx-swap="outerHTML">
    <div>
        <label>First Name</label>
        <input type="text" name="firstName" value="Joe">
    </div>
    <div class="form-group">
        <label>Last Name</label>
        <input type="text" name="lastName" value="Blow">
    </div>
    <div class="form-group">
        <label>Email Address</label>
        <input type="email" name="email" value="joe@blow.com">
    </div>
    <button class="btn">Submit</button>
    <button class="btn" hx-get="/contact/1">Cancel</button>
</form>
```

### Click to Load

This example shows how to implement click-to-load the next page in a table of data. The crux of the demo is the final row:

```html
<tr id="replaceMe">
    <td colspan="3">
        <button class='btn' hx-get="/contacts/?page=2"
                hx-target="#replaceMe"
                hx-swap="outerHTML">
            Load More Agents... <img class="htmx-indicator" src="/img/bars.svg">
        </button>
    </td>
</tr>
```

This row contains a button that will replace the entire row with the next page of results (which will contain a button to load the next page of results). And so on.

### Delete Row

This example shows how to implement a delete button that removes a table row upon completion

:

```html
<table class="table table-hover">
    <thead>
        <tr>
            <th>First</th>
            <th>Last</th>
            <th></th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Joe</td>
            <td>Blow</td>
            <td>
                <button class='btn btn-danger'
                        hx-delete="/contacts/1"
                        hx-target="closest tr"
                        hx-swap="outerHTML">
                    Delete
                </button>
            </td>
        </tr>
        <tr>
            <td>Fred</td>
            <td>Smith</td>
            <td>
                <button class='btn btn-danger'
                        hx-delete="/contacts/2"
                        hx-target="closest tr"
                        hx-swap="outerHTML">
                    Delete
                </button>
            </td>
        </tr>
        <tr>
            <td>Bill</td>
            <td>Thompson</td>
            <td>
                <button class='btn btn-danger'
                        hx-delete="/contacts/3"
                        hx-target="closest tr"
                        hx-swap="outerHTML">
                    Delete
                </button>
            </td>
        </tr>
    </tbody>
</table>
```

### Confirm Before Sending

This example shows how to implement a confirmation dialogue before making a request. The trick here is to use the confirm trigger modifier:

```html
<button hx-delete="/widget/1"
        hx-target="this"
        hx-swap="outerHTML"
        hx-trigger="click"
        hx-confirm="Are you sure you want to delete this?">
    Delete Widget
</button>
```

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

## More examples



### htmx usage with Django

There is an open-source Django/htmx project. The main aim is to show how htmx could be used in a modern, high-performance Django project. There is a lot of scope for optimization, and with Django being a highly optimized framework with a lot of built-in tools, it should be possible to produce high-quality applications without a lot of custom code. The project is based on the Django REST Framework, with a few extensions and libraries to make development easier.

The Django/htmx project is built using Django 1.11, and Python 3.6. The code is available on GitHub, and the project is licensed under the MIT license.

The project is organized into a number of apps, each of which contains a number of models, views, templates, and static files. The apps are:

- **core**: The core app contains the main models, views, templates, and static files for the project. It also contains the main URLs for the project, and the settings for the project.
- **blog**: The blog app contains the models, views, templates, and static files for the blog.
- **accounts**: The accounts app contains the models, views, templates, and static files for the accounts.
- **api**: The api app contains the models, views, templates, and static files for the API.
- **htmx**: The htmx app contains the models, views, templates, and static files for the htmx examples.
- **webpack**: The webpack app contains the models, views, templates, and static files for the webpack examples.

Each app has its own URLs, and the main URLs for the project are defined in the core app. The core app also contains the settings for the project, and the main URLs for the project.

### Extending htmx with Javascript

htmx allows you to write custom extension to add behavior that isn't natively supported in the library. You can do this using the `htmx.defineExtension` method. Here's an example of a simple extension that will log all htmx requests to the console:

```js
htmx.defineExtension('logger', {
    onEvent: function(name, evt) {
        if (name === 'htmx:beforeRequest') {
            console.log('Requesting URL: ', evt.detail.path);
        }
    }
});
```

You can then enable this extension using the `hx-ext` attribute:

```html
<div hx-get="/foo" hx-ext="logger">
    Click Me!
</div>
```

This will log the URL of every request made by htmx.

### Built-in htmx Extensions

htmx comes with a few built-in extensions that add additional behavior. Here are some of the built-in extensions:

- **ajax**: This extension adds support for making AJAX requests.
- **animation**: This extension adds support for animating elements when they are swapped.
- **csrf**: This extension adds support for including a CSRF token in all requests.
- **history**: This extension adds support for pushing URLs to the history stack.
- **intercept**: This extension adds support for intercepting and modifying requests and responses.
- **json**: This extension adds support for processing JSON responses.
- **keys**: This extension adds support for handling keyboard events.
- **method**: This extension adds support for changing the HTTP method of a request.
- **oob**: This extension adds support for out-of-band (OOB) swaps.
- **poll**: This extension adds support for polling endpoints.
- **push-url**: This extension adds support for pushing URLs to the history stack.
- **sse**: This extension adds support for Server-Sent Events.
- **swapping**: This extension adds support for swapping elements.
- **ws**: This extension adds support for WebSockets.

### Conclusion

htmx is a powerful library that enables a new way of thinking about web applications, leveraging the power of hypertext to build rich, interactive user interfaces without the complexity of modern JavaScript frameworks. By using htmx, you can create applications that are simpler, more maintainable, and more performant.

```

This content includes all the information provided and is formatted appropriately for a markdown file. You can copy and paste this into a `.md` file.