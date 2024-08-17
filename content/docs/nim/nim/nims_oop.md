---
title: "Object-Oriented Programming in Nim"
description: "Nim Lang description"
icon: "code"
draft: false
---

Nim supports object-oriented programming (OOP) principles, including member functions (methods), inheritance, and polymorphism. This guide will help you understand how to use these features effectively.

#### Member Functions (Methods)

Member functions, or methods, in Nim can be implemented using procedures (procs). Thanks to the Uniform Function Call Syntax (UFCS), you can call procs as if they were methods.

```nim
type Animal = object
  name: string
  age: int

proc speak(self: Animal, msg: string) =
  echo self.name & " says: " & msg

var sparky = Animal(name: "Sparky", age: 10)
sparky.speak("Hi")  # Method call syntax
speak(sparky, "Hi") # Traditional proc call syntax
```

UFCS allows calling procs with dot notation, making the code more readable and object-oriented.

```nim
proc double(num: int): int =
  return num * 2

echo double(10)     # Traditional call
echo 10.double()    # UFCS call
echo 10.double      # UFCS call without parentheses
```

#### Mutability in Methods

When using UFCS and procs as member functions, it's crucial to handle mutability correctly. By default, proc arguments are immutable.

```nim
type Animal = object
  name: string
  age: int

proc incAge(self: Animal) =
  self.age += 1  # Error: self.age is immutable

proc setName(self: Animal, name: string) =
  self.name = name  # Error: self.name cannot be assigned to
```

To modify the object's state, mark the argument as mutable using `var`.

```nim
proc incAge(self: var Animal) =
  self.age += 1

proc setName(self: var Animal, name: string) =
  self.name = name

var sparky = Animal(name: "Sparky", age: 3)
sparky.incAge()
sparky.setName("Spark")
```

For `ref` objects, mutability is managed through pointers, making them inherently mutable.

```nim
type Animal = ref object
  name: string
  age: int

proc incAge(self: Animal) =
  self.age += 1

var sparky = Animal(name: "Sparky", age: 3)
sparky.incAge()
```

#### Inheritance

Nim supports inheritance, allowing you to create subtypes and override methods. Use the `of` keyword to create a subtype and `method` to define dynamically dispatched methods.

```nim
type Animal = ref object of RootObj
  name: string
  age: int

method vocalize(self: Animal): string {.base.} = "..."
method ageHumanYrs(self: Animal): int {.base.} = self.age

type Dog = ref object of Animal
method vocalize(self: Dog): string = "woof"
method ageHumanYrs(self: Dog): int = self.age * 7

type Cat = ref object of Animal
method vocalize(self: Cat): string = "meow"

var animals: seq[Animal] = @[]
animals.add(Dog(name: "Sparky", age: 10))
animals.add(Cat(name: "Mitten", age: 10))

for a in animals:
  echo a.vocalize()
  echo a.ageHumanYrs()
```

Output:

```
woof
70
meow
10
```

#### Procs vs. Methods

Procs are statically dispatched, making them more performant than dynamically dispatched methods. Use methods only when you need dynamic dispatch.

#### Testing Subtypes

You can check if an object is of a given subtype using the `of` keyword.

```nim
echo(animals[0] of Dog)    # true
echo(animals[0] of Cat)    # false
echo(animals[0] of Animal) # true
```

### Summary

- **Procs**: Use for static dispatch, more performant.
- **Methods**: Use for dynamic dispatch, useful for polymorphism.
- **UFCS**: Allows calling procs with dot notation for a more OOP-like syntax.
- **Mutability**: Mark arguments as `var` for mutable access in procs.
- **Inheritance**: Use `of` keyword for creating subtypes and overriding methods.

By following these guidelines, you can leverage Nim's OOP features to write clean, efficient, and maintainable code.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
