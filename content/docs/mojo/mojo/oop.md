---
title: "Object-Oriented Programming (OOP) Concepts in Mojo"
description: "Mojo Lang description"
icon: "code"
draft: false
---

### Current State of OOP in Mojo

- As of now, Mojo is still developing its OOP capabilities. This means that traditional OOP concepts, as seen in languages like Python, are not fully implemented in Mojo. However, Mojo does offer a way to create high-level abstractions similar to objects through structures, also known as structs.

### Structures (Structs) in Mojo

- Structs in Mojo are similar to classes in Python in terms of their functionality. They support methods, fields, operator overloading, and decorators for meta-programming. However, structs in Mojo are statically bound at compile-time. This means they do not allow for dynamic dispatch or runtime changes to the structure, unlike Python classes.

### Example of a Struct in Mojo:

```mojo
struct MyPair:
    var first: Int
    var second: Int

    fn __init__(inout self, first: Int, second: Int):
        self.first = first
        self.second = second

    fn dump(self):
        print(self.first, self.second)
```

### Instantiating and Using Structs

- You can create instances of structs and use their methods. The `self` argument in Mojo is similar to Python's `self` and is used to refer to the current instance of the struct.

### Creating and Using an Instance of `MyPair`:

```mojo
let mine = MyPair(2, 4)
mine.dump() // This will print: 2 4
```

## Key Points to Remember

- Initialization: The `__init__` method in Mojo structs works similarly to constructors in OOP languages.
- Method Invocation: When calling methods like `dump()`, the `self` argument is implicitly passed as the current instance.
- Static Nature: Mojo structs are static, meaning their structure is fixed at compile time and cannot be altered during runtime.
- Future Development: Mojo plans to support classes in future releases, which may introduce more dynamic OOP features.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
