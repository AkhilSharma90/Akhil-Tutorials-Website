---
title: "Security in HTMX"
description: "Learn about what HTMX is and how you can use it."
icon: "code"
draft: false
---

htmx allows you to define logic directly in your DOM. This has a number of advantages, the largest being Locality of Behavior, which makes your system easier to understand and maintain.

A concern with this approach, however, is security: since htmx increases the expressiveness of HTML, if a malicious user is able to inject HTML into your application, they can leverage this expressiveness of htmx to malicious ends.

## Rule 1: Escape All User Content
The first rule of HTML-based web development has always been: do not trust input from the user. You should escape all 3rd party, untrusted content that is injected into your site. This is to prevent, among other issues, XSS attacks.

There is extensive documentation on XSS and how to prevent it on the excellent OWASP Website, including a Cross Site Scripting Prevention Cheat Sheet.

The good news is that this is a very old and well understood topic, and the vast majority of server-side templating languages support automatic escaping of content to prevent just such an issue.

That being said, there are times people choose to inject HTML more dangerously, often via some sort of raw() mechanism in their templating language. This can be done for good reasons, but if the content being injected is coming from a 3rd party then it must be scrubbed, including removing attributes starting with hx- and data-hx, as well as inline `<script>` tags, etc.

If you are injecting raw HTML and doing your own escaping, a best practice is to whitelist the attributes and tags you allow, rather than to blacklist the ones you disallow.

## htmx Security Tools
Of course, bugs happen and developers are not perfect, so it is good to have a layered approach to security for your web application, and htmx provides tools to help secure your application as well.

Let’s take a look at them.

### hx-disable
The first tool htmx provides to help further secure your application is the hx-disable attribute. This attribute will prevent processing of all htmx attributes on a given element, and on all elements within it. So, for example, if you were including raw HTML content in a template (again, this is not recommended!) then you could place a div around the content with the hx-disable attribute on it:

```html
<div hx-disable>
    <%= raw(user_content) %>
</div>
```
And htmx will not process any htmx-related attributes or features found in that content. This attribute cannot be disabled by injecting further content: if an hx-disable attribute is found anywhere in the parent hierarchy of an element, it will not be processed by htmx.

### hx-history
Another security consideration is htmx history cache. You may have pages that have sensitive data that you do not want stored in the users localStorage cache. You can omit a given page from the history cache by including the hx-history attribute anywhere on the page, and setting its value to false.

### Configuration Options
htmx also provides configuration options related to security:

- htmx.config.selfRequestsOnly - if set to true, only requests to the same domain as the current document will be allowed
- htmx.config.allowScriptTags - htmx will process `<script>` tags found in new content it loads. If you wish to disable this behavior you can set this configuration variable to false
- htmx.config.historyCacheSize - can be set to 0 to avoid storing any HTML in the localStorage cache
- htmx.config.allowEval - can be set to false to disable all features of htmx that rely on eval:
    - event filters
    - hx-on: attributes
    - hx-vals with the js: prefix
    - hx-headers with the js: prefix
Note that all features removed by disabling eval() can be reimplemented using your own custom javascript and the htmx event model.

### Events
If you want to allow requests to some domains beyond the current host, but not leave things totally open, you can use the htmx:validateUrl event. This event will have the request URL available in the detail.url slot, as well as a sameHost property.

You can inspect these values and, if the request is not valid, invoke preventDefault() on the event to prevent the request from being issued.
```html
document.body.addEventListener('htmx:validateUrl', function (evt) {
  // only allow requests to the current server as well as myserver.com
  if (!evt.detail.sameHost && evt.detail.url.hostname !== "myserver.com") {
    evt.preventDefault();
  }
});
```

### CSP Options
Browsers also provide tools for further securing your web application. The most powerful tool available is a Content Security Policy. Using a CSP you can tell the browser to, for example, not issue requests to non-origin hosts, to not evaluate inline script tags, etc.

Here is an example CSP in a meta tag:

```html
<meta http-equiv="Content-Security-Policy" content="default-src 'self';">
```

This tells the browser “Only allow connections to the original (source) domain”. This would be redundant with the htmx.config.selfRequestsOnly, but a layered approach to security is warranted and, in fact, ideal, when dealing with application security.

A full discussion of CSPs is beyond the scope of this document, but the MDN Article provide a good jumping off point for exploring this topic.