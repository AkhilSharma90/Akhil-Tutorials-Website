---
title: "Object Oriented Programming with Julia"
description: "Julia, a high-level, high-performance programming language, is designed for technical computing"
icon: "code"
draft: false
---
## Object-Oriented Programming in Julia

### Understanding Types and Methods

Julia, while not an object-oriented language in the traditional sense, supports composite types (similar to classes in other languages) and methods.
- **Types**: Used to define a new data structure.
- **Methods**: Functions specialized for certain types.

Example of defining a type:

```julia
struct Person
    name::String
    age::Int
end
```

### Structs and Classes

In Julia, `struct` is used to create a composite type. It’s similar to a class in other languages but is immutable by default. To create a mutable type, use `mutable struct`.

Example of a mutable struct:

```julia
mutable struct Car
    make::String
    model::String
    year::Int
end
```

### Inheritance and Polymorphism

While Julia does not support classical inheritance, it allows polymorphism through multiple dispatch. This means functions can be defined for specific types, and Julia chooses the appropriate function based on the types of all arguments.

Example of polymorphism with multiple dispatch:

```julia
# General greet function
greet(person::Person) = println("Hello, ", person.name)

# Specialized greet for children
greet(child::Child) = println("Hey, ", child.name, "!")

struct Child
    name::String
    age::Int
end

# Usage
alice = Person("Alice", 30)
bob = Child("Bob", 5)
greet(alice) # "Hello, Alice"
greet(bob) # "Hey, Bob!"
```

## Code Example: Implementing a Simple Class Structure

Let’s create a simple mutable struct and functions to demonstrate a class-like structure in Julia:

```julia
mutable struct Book
    title::String
    author::String
    pages::Int
end

# Function to create a new book
function create_book(title, author, pages)
    return Book(title, author, pages)
end

# Function to display book information
function show_book(book::Book)
    println("Book: ", book.title)
    println("Author: ", book.author)
    println("Pages: ", book.pages)
end

# Creating and displaying a book
my_book = create_book("Kanye The GOAT", "Kanye", 300)
show_book(my_book)
```

This section provides a basic understanding of how to work with composite types and methods in Julia, mimicking an object-oriented style.