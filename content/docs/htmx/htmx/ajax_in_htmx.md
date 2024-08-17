---
title: "AJAX in HTMX"
description: "Learn about what HTMX is and how you can use it."
icon: "code"
draft: false
---

The core of htmx is a set of attributes that allow you to issue AJAX requests directly from HTML:

| Attribute | Description                              |
| --------- | ---------------------------------------- |
| hx-get    | Issues a GET request to the given URL    |
| hx-post   | Issues a POST request to the given URL   |
| hx-put    | Issues a PUT request to the given URL    |
| hx-patch  | Issues a PATCH request to the given URL  |
| hx-delete | Issues a DELETE request to the given URL |

Each of these attributes takes a URL to issue an AJAX request to. The element will issue a request of the specified type to the given URL when the element is triggered:

```html
<div hx-put="/messages">Put To Messages</div>
```

This tells the browser:  
When a user clicks on this div, issue a PUT request to the URL /messages and load the response into the div.

## Triggers

### Triggering Requests

By default, AJAX requests are triggered by the “natural” event of an element:

- input, textarea & select are triggered on the change event
- form is triggered on the submit event
- everything else is triggered by the click event

If you want different behavior you can use the hx-trigger attribute to specify which event will cause the request.

Here is a div that posts to /mouse_entered when a mouse enters it:

```html
<div hx-post="/mouse_entered" hx-trigger="mouseenter">[Here Mouse, Mouse!]</div>
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
- delay:`<time interval>` - wait the given amount of time (e.g. 1s) before issuing the request. If the event triggers again, the countdown is reset.
- throttle:`<time interval>` - wait the given amount of time (e.g. 1s) before issuing the request. Unlike delay if a new event occurs before the time limit is hit the event will be discarded, so the request will trigger at the end of the time period.
- from:`<CSS Selector>` - listen for the event on a different element. This can be used for things like keyboard shortcuts.

You can use these attributes to implement many common UX patterns, such as Active Search:

```html
<input
  type="text"
  name="q"
  hx-get="/trigger_delay"
  hx-trigger="keyup changed delay:500ms"
  hx-target="#search-results"
  placeholder="Search..."
/>
<div id="search-results"></div>
```

This input will issue a request 500 milliseconds after a key up event if the input has been changed and inserts the results into the div with the id search-results. Multiple triggers can be specified in the hx-trigger attribute, separated by commas.

### Trigger Filters

You may also apply trigger filters by using square brackets after the event name, enclosing a javascript expression that will be evaluated. If the expression evaluates to true the event will trigger, otherwise it will not.

Here is an example that triggers only on a Control-Click of the element

```html
<div hx-get="/clicked" hx-trigger="click[ctrlKey]">Control Click Me</div>
```

Properties like ctrlKey will be resolved against the triggering event first, then the global scope. The this symbol will be set to the current element.

### Special Events

htmx provides a few special events for use in hx-trigger:

- load - fires once when the element is first loaded
- revealed - fires once when an element first scrolls into the viewport
- intersect - fires once when an element first intersects the viewport. This supports two additional options: - root:`<selector>` - a CSS selector of the root element for intersection - threshold:`<float>` - a floating point number between 0.0 and 1.0, indicating what amount of intersection to fire the event on
  You can also use custom events to trigger requests if you have an advanced use case.

### Polling

If you want an element to poll the given URL rather than wait for an event, you can use the every syntax with the hx-trigger attribute:

```html
<div hx-get="/news" hx-trigger="every 2s"></div>
```

This tells htmx

Every 2 seconds, issue a GET to /news and load the response into the div

If you want to stop polling from a server response you can respond with the HTTP response code 286 and the element will cancel the polling.

### Load Polling

Another technique that can be used to achieve polling in htmx is “load polling”, where an element specifies a load trigger along with a delay, and replaces itself with the response:

```html
<div hx-get="/messages" hx-trigger="load delay:1s" hx-swap="outerHTML"></div>
```

If the /messages end point keeps returning a div set up this way, it will keep “polling” back to the URL every second.

Load polling can be useful in situations where a poll has an end point at which point the polling terminates, such as when you are showing the user a progress bar.

## Indicators

### Request Indicators

When an AJAX request is issued it is often good to let the user know that something is happening since the browser will not give them any feedback. You can accomplish this in htmx by using htmx-indicator class.

The htmx-indicator class is defined so that the opacity of any element with this class is 0 by default, making it invisible but present in the DOM.

When htmx issues a request, it will put a htmx-request class onto an element (either the requesting element or another element, if specified). The htmx-request class will cause a child element with the htmx-indicator class on it to transition to an opacity of 1, showing the indicator.

```html
<button hx-get="/click">
  Click Me!
  <img class="htmx-indicator" src="/spinner.gif" />
</button>
```

Here we have a button. When it is clicked the htmx-request class will be added to it, which will reveal the spinner gif element. (I like SVG spinners these days.)

While the htmx-indicator class uses opacity to hide and show the progress indicator, if you would prefer another mechanism you can create your own CSS transition like so:

```html
.htmx-indicator{ display:none; } .htmx-request .htmx-indicator{ display:inline;
} .htmx-request.htmx-indicator{ display:inline; }
```

If you want the htmx-request class added to a different element, you can use the hx-indicator attribute with a CSS selector to do so:

```html
<div>
  <button hx-get="/click" hx-indicator="#indicator">Click Me!</button>
  <img id="indicator" class="htmx-indicator" src="/spinner.gif" />
</div>
```

Here we call out the indicator explicitly by id. Note that we could have placed the class on the parent div as well and had the same effect.

You can also add the the disabled attribute to elements for the duration of a request by using the hx-disabled-elt attribute.

## Targets

If you want the response to be loaded into a different element other than the one that made the request, you can use the hx-target attribute, which takes a CSS selector. Looking back at our Live Search example:

```html
<input
  type="text"
  name="q"
  hx-get="/trigger_delay"
  hx-trigger="keyup delay:500ms changed"
  hx-target="#search-results"
  placeholder="Search..."
/>
<div id="search-results"></div>
```

You can see that the results from the search are going to be loaded into div#search-results, rather than into the input tag.

## Extended CSS Selectors

hx-target, and most attributes that take a CSS selector, support an “extended” CSS syntax:

- You can use the this keyword, which indicates that the element that the hx-target attribute is on is the target
- The closest <CSS selector> syntax will find the closest ancestor element or itself, that matches the given CSS selector. (e.g. closest tr will target the closest table row to the element)
- The next <CSS selector> syntax will find the next element in the DOM matching the given CSS selector.
- The previous <CSS selector> syntax will find the previous element in the DOM the given CSS selector.
- find <CSS selector> which will find the first child descendant element that matches the given CSS selector. (e.g find tr would target the first child descendant row to the element)
  In addition, a CSS selector may be wrapped in < and /> characters, mimicking the query literal syntax of hyperscript.

Relative targets like this can be useful for creating flexible user interfaces without peppering your DOM with loads of id attributes.

## Synchronization

Often you want to coordinate the requests between two elements. For example, you may want a request from one element to supersede the request of another element, or to wait until the other element’s request has finished.

htmx offers a hx-sync attribute to help you accomplish this.

Consider a race condition between a form submission and an individual input’s validation request in this HTML:

```html
<form hx-post="/store">
  <input
    id="title"
    name="title"
    type="text"
    hx-post="/validate"
    hx-trigger="change"
  />
  <button type="submit">Submit</button>
</form>
```

Without using hx-sync, filling out the input and immediately submitting the form triggers two parallel requests to /validate and /store.

Using hx-sync="closest form:abort" on the input will watch for requests on the form and abort the input’s request if a form request is present or starts while the input request is in flight:

```html
<form hx-post="/store">
  <input
    id="title"
    name="title"
    type="text"
    hx-post="/validate"
    hx-trigger="change"
    hx-sync="closest form:abort"
  />
  <button type="submit">Submit</button>
</form>
```

This resolves the synchronization between the two elements in a declarative way.

htmx also supports a programmatic way to cancel requests: you can send the htmx:abort event to an element to cancel any in-flight requests:

```html
<button id="request-button" hx-post="/example">Issue Request</button>
<button onclick="htmx.trigger('#request-button', 'htmx:abort')">
  Cancel Request
</button>
```

### Click to Edit

The click to edit pattern provides a way to offer inline editing of all or part of a record without a page refresh.

- This pattern starts with a UI that shows the details of a contact. The div has a button that will get the editing UI for the contact from /contact/1/edit

```html
<div hx-target="this" hx-swap="outerHTML">
  <div><label>First Name</label>: Joe</div>
  <div><label>Last Name</label>: Blow</div>
  <div><label>Email</label>: joe@blow.com</div>
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
    <input type="text" name="firstName" value="Joe" />
  </div>
  <div class="form-group">
    <label>Last Name</label>
    <input type="text" name="lastName" value="Blow" />
  </div>
  <div class="form-group">
    <label>Email Address</label>
    <input type="email" name="email" value="joe@blow.com" />
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
    <button
      class="btn"
      hx-get="/contacts/?page=2"
      hx-target="#replaceMe"
      hx-swap="outerHTML"
    >
      Load More Agents... <img class="htmx-indicator" src="/img/bars.svg" />
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
        <button
          class="btn btn-danger"
          hx-delete="/contacts/1"
          hx-target="closest tr"
          hx-swap="outerHTML"
        >
          Delete
        </button>
      </td>
    </tr>
    <tr>
      <td>Fred</td>
      <td>Smith</td>
      <td>
        <button
          class="btn btn-danger"
          hx-delete="/contacts/2"
          hx-target="closest tr"
          hx-swap="outerHTML"
        >
          Delete
        </button>
      </td>
    </tr>
    <tr>
      <td>Bill</td>
      <td>Thompson</td>
      <td>
        <button
          class="btn btn-danger"
          hx-delete="/contacts/3"
          hx-target="closest tr"
          hx-swap="outerHTML"
        >
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
<button
  hx-delete="/widget/1"
  hx-target="this"
  hx-swap="outerHTML"
  hx-trigger="click"
  hx-confirm="Are you sure you want to delete this?"
>
  Delete Widget
</button>
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
