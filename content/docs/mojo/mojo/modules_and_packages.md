---
title: "Modules and Packages in Mojo"
description: "Mojo Lang description"
icon: "code"
draft: false
---

## Overview
Modules and packages in Mojo, while having some similarities, differ significantly from Python's
approach. Understanding these differences is key to leveraging Mojo effectively, especially as
the language continues to evolve and adopt a robust package management system.
## Mojo Files and Main Function
In Mojo, like in many programming languages, a main function is essential for executing code.
This is evident when creating and running Mojo files.
Example of a Mojo File (hello.mojo):
```mojo
fn main():
print("Hello, world!")
```

To run this file, use the command: mojo hello.mojo.
### Scripting with Arguments
Suppose you want to turn a script into a calculator that adds numbers passed as arguments. In
this case, the approach of using a main function in Mojo files remains valid.
### Creating Modules
When building reusable code with structures and methods, a different approach is needed,
particularly when the code doesn't depend on a main function.
Creating a Module (mymodule.mojo):
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

### Using Modules in Mojo Files
You can include and use modules in your main Mojo file through various import methods.
Importing and Using a Module in hello.mojo:
```mojo
# Option 1: Specific Import
from mymodule import MyPair
fn main():
let mine = MyPair(2, 4)
mine.dump()
# Option 2: Generic Import
import mymodule
fn main():
let mine = mymodule.MyPair(2, 4)
mine.dump()
# Option 3: Import with Alias
import mymodule as my
fn main():
let mine = my.MyPair(2, 4)
mine.dump()
```
## Mojo Packages
Packages in Mojo are used to organize and distribute a collection of related modules.
Example Package Structure:
```mojo
main.mojo
mypackage/
__init__.mojo
mymodule.mojo
```

Using a Package in main.mojo:
```mojo
from mypackage.mymodule import MyPair
fn main():
let mine = MyPair(2, 4)
mine.dump()
```

Creating a Mojo Package:


To bundle your package for distribution, you can use the Mojo package command:
`mojo package mypackage -o mypack.mojopkg`            

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
