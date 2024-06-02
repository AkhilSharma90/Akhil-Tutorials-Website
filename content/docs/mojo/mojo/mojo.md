---
title: "Mojo"
description: "Mojo Lang description"
icon: "code"
draft: false
---

# Why Mojo

## Background

- **Common Perception:** Python is often labeled as a slow and merely a scripting language.
- **Frustration with Misconception:** The belief that Python's performance issues stem from the language itself, rather than how the code is written.

## Choosing Mojo: The Main Reason

- **Modular | Mojo as a Bridge:** Mojo combines Python's ease of use with advanced programming capabilities. It's tailored for both research and production environments.
- **Systems Programming Language:** Designed for heterogeneous computing, Mojo excels in handling different processors like CPUs, GPUs, FPGAs, and NPUs.
- **Versatility and Control:** As an AI Application Research Engineer, the author values complete control over the entire process - from research to deployment, without needing extensive hardware knowledge or multiple tools.

## Key Takeaway

- Mojo is not just about enhancing speed; it's about offering a comprehensive solution that balances ease of use with sophisticated programming capabilities for diverse computing environments.

# Getting Started

## Initial Setup

- **Download Mojo SDK:** Essential for running Mojo code.
  - Includes the "Mojo CLI" (Command Line Interface), a versatile tool for executing Mojo code and other tasks.
  - Provides a REPL (Read-Eval-Print Loop) environment for interactive coding.

## Learning and Development Environment

- **Using REPL:** Ideal for beginners, similar to a basic calculator, great for practicing small code snippets.
- **Advancing with Mojo:**
  - Organize code into Mojo files, modules, and packages.
  - Use Visual Studio Code (VSCode) for a more structured coding environment.
  - Leverage a Mojo extension for VSCode offering features like auto-completion and quick fixes.

## Installation on Different Operating Systems

- **Compatibility:** Currently, Mojo SDK is only compatible with Ubuntu & MacOS. Windows users can utilize WSL (Windows Subsystem for Linux) container.
- **Installation Steps:**
  - For Linux or MacOS:
    ```bash
    curl https://get.modular.com | MODULAR_AUTH=mut_e982ece66e6949d593f64xxxx sh -
    modular install mojo
    ```
  - **Setting Environment Variables:** Add MODULAR_HOME and PATH to .bashrc or .zsh files as suggested during installation.

## Updating Mojo SDK

- Run the following commands:
  ```bash
  sudo apt-get update
  sudo apt-get install modular
  modular clean
  modular install mojo
  ```

## Visual Studio Code Extension

- Download the VSCode extension for an enhanced Mojo coding experience.

## Mojo Playground

- Access the Mojo environment in a browser via the Mojo playground link.

# Running Mojo Code

## Using REPL

- Start REPL by typing `mojo`.
- Example:
  ```
  mojo
  # Type your code here
  ```

## Mojo Source File

- Create a file (e.g., hello.mojo) and write your Mojo code.
- Example:
  ```mojo
  fn main():
      print("Hello, world!")
  ```

## Executing Code

- Run a Mojo file: `mojo hello.mojo`
- Build an executable: `mojo build hello.mojo`
- Run the executable: `./hello`

## Mojo CLI Usage

- **Basic Commands:**
  - Check version: `mojo --version` or `mojo -v`
  - Get help: `mojo --help` or `mojo -h`
  - Run a file: `mojo run ./hello.mojo`
  - Build a file: `mojo build ./hello.mojo`
  - Launch REPL: `mojo repl`
  - Other commands include debug, package, format, doc, and demangle.

# Data Types, Variables, and Operators in Mojo

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

## Functions in Mojo

### Function Declaration

- In Mojo, functions can be declared using `fn` for strongly-typed, memory-safe behavior, or `def` for dynamic behavior similar to Python. However, Mojo's support for keyword arguments is still a work in progress.

### Keyword Parameters Example:

```mojo
fn foo[a: Int, b: Int = 42]():
    print(a, "+", b)

foo[a=5]()        // prints '5 + 42'
foo[a=7, b=13]()  // prints '7 + 13'
foo[b=20, a=6]()  // prints '6 + 20'
```

### Struct with Keyword Parameters

```mojo
struct KwParamStruct[a: Int, msg: String = "mojo"]:
    fn __init__(inout self):
        print(msg, a)

fn use_kw_params():
    KwParamStruct[a=42]()         // prints 'mojo 42'
    KwParamStruct[5, msg="hello"]()  // prints 'hello 5'
    KwParamStruct[msg="hello", a=42]()  // prints 'hello 42'
```

## Memory Management with Pointers

### Example of a Struct in Mojo with Pointers:

```mojo
struct HeapArray:
    var data: Pointer[Int]
    var size: Int

    fn __init__(inout self, size: Int, val: Int):
        self.size = size
        self.data = Pointer[Int].alloc(self.size)
        for i in range(self.size):
            self.data.store(i, val)

    fn __del__(owned self):
        self.data.free()

    fn dump(self):
        print_no_newline("[")
        for i in range(self.size):
            if i > 0:
                print_no_newline(", ")
            print_no_newline(self.data.load(i))
        print("]")
```

# Copying Objects
Implementing a copy constructor (__copyinit__) allows for object copying without errors.

```mojo
struct HeapArray:
#... (previous code)
fn __copyinit__(inout self, other: Self):
self.size = other.size
self.data = Pointer[Int].alloc(self.size)
for i in range(self.size):
self.data.store(i, other.data.load(i))
var a = HeapArray(3, 1)
a.dump() # [1, 1, 1]
var b = a
b.dump() # [1, 1, 1]
```

## Working with Structs and Methods
```mojo
struct SomethingBig:
var id_number: Int
var huge: HeapArray
fn __init__(inout self, id: Int):
self.huge = HeapArray(1000, 0)
self.id_number = id
fn set_id(inout self, number: Int):
self.id_number = number
fn print_id(self): # Same as: fn print_id(borrowed self):
print(self.id_number)
fn use_something_big(borrowed a: SomethingBig, b: SomethingBig):
a.print_id()
b.print_id()
let a = SomethingBig(10)
let b = SomethingBig(20)
use_something_big(a, b)
```

## Object-Oriented Programming (OOP) Concepts in Mojo

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

## Interoperability with Python

### Importing Python Modules in Mojo

- Mojo's ability to import and use Python modules is a significant advantage, especially for leveraging existing Python code. It utilizes the CPython interpreter, allowing seamless integration with Python modules.

### Example: Using NumPy in Mojo

```mojo
from python import Python
let np = Python.import_module("numpy")
ar = np.arange(15).reshape(3, 5)
print(ar.shape)
```

- Note: NumPy must be installed in your Python environment.

### Limitations

- While Mojo can import Python modules, it is not yet a feature-complete superset of Python. Therefore, not all Python code can be directly copied and run in Mojo.

## Running Python Code Examples in Mojo

### 1. Basic Calculator Example

```python
def add(x, y):
    return x + y

def subtract(x, y):
    return x - y

def multiply(x, y):
    return x * y

def divide(x, y):
    return x / y

print("Select operation.")
print("1. Add")
print("2. Subtract")
print("3. Multiply")
print("4. Divide")

while True:
    choice = input("Enter choice (1/2/3/4): ")

    if choice in ('1', '2', '3', '4'):
        num1 = float(input("Enter first number: "))
        num2 = float(input("Enter second number: "))

        if choice == '1':
            print(num1, "+", num2, "=", add(num1, num2))
        elif choice == '2':
            print(num1, "-", num2, "=", subtract(num1, num2))
        elif choice == '3':
            print(num1, "*", num2, "=", multiply(num1, num2))
        elif choice == '4':
            print(num1, "/", num2, "=", divide(num1, num2))
        break
    else:
        print("Invalid Input")
```

### 2. Using Tabulate Library

```mojo
// Ensure Python tabulate is installed
from python import Python
let tabulate = Python.import_module("tabulate")

let text_data = """
Name Age Occupation
Alice 25 Engineer
Bob 30 Developer
Charlie 40 Manager
"""

let rows = [row.strip().split() for row in text_data.strip().split("\n")]
let table = tabulate.tabulate(rows, headers="firstrow")
print(table)
```

### 3. Text Extraction from Images with Tesseract OCR

```mojo
// Ensure Python pytesseract and Pillow are installed
import pytesseract
from PIL import Image

// Configure the Tesseract command if not in PATH
pytesseract.pytesseract.tesseract_cmd = r'path_to_tesseract.exe'

def read_image_text(image_path):
    image = Image.open(image_path)
    text = pytesseract.image_to_string(image)
    return text

// Usage Example
let image_path = "path_to_image.png"
let text = read_image_text(image_path)
print(text)
```

## Object-Oriented Programming (OOP) Concepts in Mojo

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