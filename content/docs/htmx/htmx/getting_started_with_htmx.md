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
