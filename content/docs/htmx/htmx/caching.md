---
title: "Caching"
description: "Learn about what HTMX is and how you can use it."
icon: "code"
draft: false
---

htmx works with standard HTTP caching mechanisms out of the box.

If your server adds the Last-Modified HTTP response header to the response for a given URL, the browser will automatically add the If-Modified-Since request HTTP header to the next requests to the same URL. Be mindful that if your server can render different content for the same URL depending on some other headers, you need to use the Vary response HTTP header. For example, if your server renders the full HTML when the HX-Request header is missing or false, and it renders a fragment of that HTML when HX-Request: true, you need to add Vary: HX-Request. That causes the cache to be keyed based on a composite of the response URL and the HX-Request request header — rather than being based just on the response URL.

If you are unable (or unwilling) to use the Vary header, you can alternatively set the configuration parameter getCacheBusterParam to true. If this configuration variable is set, htmx will include a cache-busting parameter in GET requests that it makes, which will prevent browsers from caching htmx-based and non-htmx based responses in the same cache slot.

htmx also works with ETag as expected. Be mindful that if your server can render different content for the same URL (for example, depending on the value of the HX-Request header), the server needs to generate a different ETag for each content.