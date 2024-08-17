---
title: "API Architecture: Design Best Practices for REST APIs"
description: "If you are looking to grow in you tech career and understand system design indepth, this guide is for you."
icon: "code"
draft: false
---

Web services have existed as long as the HTTP protocol itself. However, with the advent of cloud computing, they have become the ubiquitous method for enabling client interaction with services and data.

As a developer, I’ve had the opportunity to work with various SOAP services. However, my primary focus has been on REST, a resource-based architectural style for developing APIs and web services. Throughout my career, I’ve been involved in numerous projects where I’ve built, designed, and utilized APIs. While many APIs claim to be "RESTful," adhering to the principles and constraints of REST architecture, some fall short, leading to poor implementations.

I’ve encountered several poorly designed REST APIs with inaccurate usage of HTTP status codes, plain text responses, inconsistent schemas, and verbs in endpoints. These experiences prompted me to compile a set of best practices for designing REST APIs.

## Key Best Practices for REST API Design

### 1. Master the Basics of HTTP

Understanding the basics of the HTTP protocol is crucial for designing a well-structured REST API. The Mozilla Developer Network offers a comprehensive overview of HTTP. Here’s a summary of HTTP concepts applied to RESTful design:

- **HTTP Verbs**: GET, POST, PUT, PATCH, DELETE.
- **Resources and URIs**: REST is resource-oriented, and resources are represented by URIs (e.g., /library/).
- **Endpoints**: A combination of a verb and a URI (e.g., GET: /books/).
- **CRUD Operations**: Verbs map to CRUD operations (GET=Read, POST=Create, PUT/PATCH=Update, DELETE=Delete).
- **Status Codes**: Indicate the response’s status (1xx=Informational, 2xx=Success, 3xx=Redirection, 4xx=Client Error, 5xx=Server Error).

### 2. Avoid Returning Plain Text

Although not mandated by REST architectural style, most REST APIs conventionally use JSON for data exchange. Ensure to specify the `Content-Type` header as `application/json` to aid accurate decoding by programmatic clients.

### 3. Avoid Using Verbs in URIs

HTTP verbs should sufficiently describe the action performed on a resource. For example, instead of `GET: /books/:slug/generateBookCover/`, use `GET: /books/:slug/bookCover/`. Similarly, replace `POST: /books/createNewBook/` with `POST: /books/`.

### 4. Use Plural Nouns for Resources

Consistently use plural nouns for resource URIs to prevent ambiguity. For example, use `/books/:id/` instead of `/book/:id/`.

### 5. Return Error Details in the Response Body

When handling errors, return detailed error information in the JSON response body to aid debugging. For example:

```json
{
  "error": "Invalid payload.",
  "detail": {
    "name": "This field is required."
  }
}
```

### 6. Pay Attention to HTTP Status Codes

Returning appropriate HTTP status codes is critical. For instance, avoid returning `200 OK` for error responses. Use meaningful status codes like `400 Bad Request` for client errors.

### 7. Use HTTP Status Codes Consistently

Ensure consistent use of HTTP status codes across endpoints. For example:

- `GET`: `200 OK`
- `PUT`: `200 OK`
- `POST`: `201 Created`
- `PATCH`: `200 OK`
- `DELETE`: `204 No Content`

### 8. Avoid Nesting Resources

Nesting resources can complicate URIs. Instead of `GET: /authors/Cagan/books/`, use query parameters like `GET: /books?author=Cagan`.

### 9. Handle Trailing Slashes Gracefully

Choose a convention for trailing slashes and stick to it. Implement redirects to handle clients using the wrong convention.

### 10. Use Query Strings for Filtering and Pagination

Utilize query strings for filtering and pagination. For example:

- Pagination: `GET: /books?page=1&page_size=10`
- Filtering: `GET: /books?published=true&page=2&page_size=10`

### 11. Differentiate Between 401 Unauthorized and 403 Forbidden

Use `401 Unauthorized` when authentication credentials are missing or invalid. Use `403 Forbidden` when the user lacks necessary permissions.

### 12. Utilize HTTP 202 Accepted

Use `202 Accepted` when the request is accepted but not yet processed. This is useful for asynchronous processing scenarios.

### 13. Use Specialized Web Frameworks for REST APIs

Choose frameworks specifically designed for REST APIs to ensure best practices are followed effortlessly. For Python, frameworks like Falcon and Django REST Framework are excellent choices. For Node.js, consider Restify.

## Conclusion

Striving to design APIs that are a pleasure to use benefits both consumers and developers. The principles of good semantics, simplicity, and common sense are at the heart of effective REST API design. REST API design is more of an art than a science—the more you practice, the better you get. If you have alternative approaches to the tips shared above, I would love to hear them. In the meantime, keep building and improving those APIs!

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
