---
title: "Functions in Mojo"
description: "Mojo Lang description"
icon: "code"
draft: false
---

## Function Declaration

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

## Copying Objects

Implementing a copy constructor (**copyinit**) allows for object copying without errors.

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

## Operator Overloading

Mojo supports operator overloading with methods like **add** and **iadd**.

```mojo
struct MyInt:
var value: Int
#... (previous constructor and copy constructor)
fn __add__(self, rhs: MyInt) -> MyInt:return MyInt(self.value + rhs.value)
fn __iadd__(inout self, rhs: Int):
self = self + rhs
var x: MyInt = 42
x += 1
print(x.value) # prints 43
```

## Function Parameters and Mutability

Demonstrating the use of inout for mutable parameters and function overloading.

```mojo
fn swap(inout lhs: Int, inout rhs: Int):
let tmp = lhs
lhs = rhs
rhs = tmp
var x = 42
var y = 12
swap(x, y)
print(x, y) # Prints 12, 42
```

## Unique Pointers and Ownership

Modeling unique pointer behavior in Mojo for advanced memory management.

```mojo
struct UniquePointer:
var ptr: Int
fn __init__(inout self, ptr: Int):
self.ptr = ptr
fn __moveinit__(inout self, owned existing: Self):
self.ptr = existing.ptr
fn __del__(owned self):
self.ptr = 0
fn take_ptr(owned p: UniquePointer):
print("take_ptr")print(p.ptr)
fn use_ptr(borrowed p: UniquePointer):
print("use_ptr")
print(p.ptr)
fn work_with_unique_ptrs():
let p = UniquePointer(100)
use_ptr(p) # borrowing function call
take_ptr(p^) # passing ownership
# use_ptr(p) # ERROR: p is no longer valid here!
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
