---
title: "AJAX in HTMX"
description: "Learn about what HTMX is and how you can use it."
icon: "code"
draft: false
---

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
