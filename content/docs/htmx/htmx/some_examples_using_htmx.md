---
title: "Learning More Abuot HTMX"
description: "Learn about what HTMX is and how you can use it."
icon: "code"
draft: false
---


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
