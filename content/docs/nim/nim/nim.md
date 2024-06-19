---
title: "An Introduction to Nim"
description: "Nim Lang description"
icon: "code"
draft: false
---

Nim is a relatively new programming language which allows users to write easy-to-read high-performance code.

**Installation**
• Nim has ready made distributions for all three major operating systems and there are several options when it comes to installing Nim.

• You can follow the [official installation procedure](https://nim-lang.org/index/html) to install the latest stable version.

• If you’re using Linux, there is a high probability that your distribution has Nim in the package manager. If you are installing it that way, make sure it’s the most recent version.

## Testing the installing
To check if the installation was successful, we will write a program which is traditionally used as an introductory example: Hello World.

In a new text file called e.g. helloworld.nim we need to write just one line of code:

```nim
echo "Hello World!"
```
First we need to compile our program, and then run it to see if it works as expected.
Open your terminal in the same directory where your file is (on Linux you can get "Open Terminal here" if you right-click the directory in your file manager, on Windows you should use Shift + right-click to get the menu option for opening the command line).

We compile our program by typing in the terminal:
```nim
nim c helloworld.nim
```

After a successful compilation, we can run our program. On Linux we can run our program by typing ./helloworld in the terminal, and on Windows we do it by typing helloworld.exe.
There is also a possibility to both compile and run the program with just one command. We need to type:
```nim
nim c -r helloworld.nim
```
If you’re using VSCode with the Code Runner extension mentioned before, you’ll just have to press Ctrl+Alt+N and your file will be compiled and run.

Whichever way you chose to run your program, after a brief moment in the output window (or in your terminal) you should see:
Hello World!.

## Variable declaretion
Nim is a statically typed programming language, meaning that the type of an assignment needs to be declared before using the value.

In Nim we also distinguish values that can change, or mutate, from those that can’t, but more on this later. We can declare a variable (a mutable assignment) using the var keyword, just by stating its name and type (the value can be added later) by using this syntax:

`var <name>: <type>`

If we already know its value, we can declare a variable and give it a value immediately:

`var <name>: <type> = <value>`

We can assign a variable without an explicit type like this:

`var <name> = <value>`

An example of this in Nim looks like this:
```nim
var a: int
var b = 7
```
As previously mentioned variables are mutable, i.e. their value can change (multiple times), but their type must stay the same as declared.

```nim
var f = 7           
f = -3              
f = 19
f = "Hello" # error
```

## Immutable assignment
Unlike variables declared with var keyword, two more types of assignment exist in Nim, whose value cannot change, one declared with the const keyword, and the other declared with the let keyword.

**Const**

The value of an immutable assignment declared with const keyword must be known at compile time (before the program is run).
For example, we can declare the acceleration of gravity as const g = 9.81 or pi as const pi = 3.14, as we know their values in advance and these values will not change during the execution of our program.
```nim
const g = 35
g = -27         # error 

var h = -5
const i = h + 7 # error
```

In some programming languages it is a common practice to have the names of constants written in ALL_CAPS. Constants in Nim are written just like any other variable.

**Let**

Immutable assignments declared with let don’t need to be known at compile time, their value can be set at any time during the execution of a program, but once it is set, their value cannot change.

```nim
let j = 35
j = -27 # error 

var k = -5
let l = k + 7  
```


In practice, you will see/use let more frequently than const.
While you could use var for everything, your default choice should be let. Use var only for the variables which will be modified.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
