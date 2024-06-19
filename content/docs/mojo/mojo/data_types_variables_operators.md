---
title: "Data Types, Variables, and Operators in Mojo"
description: "Mojo Lang description"
icon: "code"
draft: false
---

## Understanding Data in Mojo

- Data is essential in any programming language, including Mojo. It can be anything from numbers and text to complex types like images and audio files. These data are stored in variables, data structures, or files for computation and generating outputs.

## Examples of Data Declaration in Mojo:

- `name = "Vibs"` (Using an emoji as a variable)
- `x = 1`
- `movie = "Hackerman.mp4"`
- `addresses = ["Malibu", "Brooklyn"]`
- `zipcode = ("90265", "11203")`
- `print(x)`

## Variables and Data Structures:

```mojo
a = 1
print("type of var a:", type(a))
b = 1.0
print("type of var b:", type(b))
c = "Hello World"
print("type of var c:", type(c))
d = (1, 2)
print("type of var d:", type(d))
e = ["CA", "OR"]
print("type of var e:", type(e))
f = {"a": 1, "b": 2}
print("type of var f:", type(f))
```

## Value Semantics in Mojo

- Mojo supports value semantics, which focuses on the value of an object rather than its identity. This means you can pass data structures as logical copies, not references.

## Data Types in Mojo

- Data types in Mojo are classifications that tell the compiler how to process and use the data. Understanding them is crucial for efficient and correct operations.

## Examples:

```mojo
let x: Int = 4
print(x)
let y: Int = 6
print(y)
```

## Computer Memory and Its Impact

- Understanding computer memory structure, such as CPU cache levels and the distinction between the stack and the heap, is beneficial for AI Engineers, especially for optimizing code performance.

## Variables and Constants

- In Mojo, variables can be declared as mutable using `var` or immutable using `let`.

## Mutable vs Immutable Variables:

```mojo
var x = 1
x = 2 // Works fine
let y = 1
// y = 2 // This would cause an error
```

## Standard Data Types in Mojo

- Mojo has a range of built-in data types available through modules, such as Int, Bool, String, and Tuple. These types are implemented using structs in Mojo.

## Advanced Data Types and Structures

- `StringLiteral` vs `String`: `StringLiteral` is constant and loaded into read-only memory. `String` is a heap-allocated type for larger data storage.
- Dynamic Vectors: Similar to lists in Python, used for storing multiple values.
- Tuple: A collection of different data types, accessed by their index.

## Example of Tuple:

```mojo
let tup = (1, "Mojo", 3)
print(tup.get0, Int) // Prints: 1
```

## SIMD (Single Instruction, Multiple Data)

- SIMD in Mojo allows performing the same operation across a vector in a single instruction, which enhances performance.

### Example of SIMD:

```mojo
from DType import DType
y = SIMD[DType.uint8, 4](1, 2, 3, 4)
y *= 10
print(y)
```

## Structs vs Classes in Mojo

- Structs in Mojo are similar to classes in Python in terms of supporting methods, fields, and operator overloading. However, Mojo structs are static and compile-time bound, unlike Python classes.

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

let mine = MyPair(2, 4)
mine.dump() // Prints values of the pair
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
