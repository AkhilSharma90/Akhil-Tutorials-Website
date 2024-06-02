---
title: "Modules in Nim"
description: "Learn more about nim's module system"
icon: "code"
draft: false
---

## Modules

So far we have used the functionality which is available by default every time we start a new Nim file. This can be extended with modules, which give more functionality for some specific topics.

Some of the most used Nim modules are:

- `strutils`: additional functionality when dealing with strings
- `sequtils`: additional functionality for sequences
- `math`: mathematical functions (logarithms, square roots, …), trigonometry (sin, cos, …)
- `times`: measure and deal with time

But there are many more, both in what’s called the standard library and in the Nimble package manager.

### Importing a module

If we want to import a module and all of its functionality, all we have to do is put `import <moduleName>` in our file. This is commonly done at the top of the file so we can easily see what our code uses.

#### Example: `stringutils.nim`

```nim
import strutils

let
  a = "My string with whitespace."
  b = '!'

echo a.split()
echo a.toUpperAscii()
echo b.repeat(5)
```

- Importing `strutils`.
- Using `split` from `strutils` module. It splits the string into a sequence of words.
- `toUpperAscii` converts all ASCII letters to uppercase.
- `repeat` is also from `strutils` module, and it repeats either a character or a whole string the requested amount of times.

Output:

```
@["My", "string", "with", "whitespace."]
MY STRING WITH WHITESPACE.
!!!!!
```

To users coming from other programming languages (especially Python), the way that imports work in Nim might seem "wrong".

#### Example: `maths.nim`

```nim
import math

let
  c = 30.0 # degrees
  cRadians = c.degToRad()

echo cRadians
echo sin(cRadians).round(2)

echo 2^5
```

- Importing `math`.
- Converting degrees to radians with `degToRad`.
- `sin` takes radians. We round (also from `math` module) the result to at most 2 decimal places. (Otherwise the result would be: 0.4999999999999999)
- Math module also has `^` operator for calculating powers of a number.

Output:

```
0.5235987755982988
0.5
32
```

## Creating our own

Often times we have so much code in a project that it makes sense to split it into pieces that each do a certain thing. If you create two files side by side in a folder, let’s call them `firstFile.nim` and `secondFile.nim`, you can import one from the other as a module:

#### `firstFile.nim`

```nim
proc plus*(a, b: int): int =
  return a + b

proc minus(a, b: int): int =
  return a - b
```

Notice how the `plus` procedure now has an asterisk (\*) after its name; this tells Nim that another file importing this one will be able to use this procedure.

By contrast, this will not be visible when importing this file.

#### `secondFile.nim`

```nim
import firstFile

echo plus(5, 10)
echo minus(10, 5) # error
```

Here we import `firstFile.nim`. We don’t need to put the `.nim` extension on here.

This will work fine and output `15` as it’s declared in `firstFile` and visible to us.

However, this will throw an error as the `minus` procedure is not visible since it doesn’t have an asterisk behind its name.

In case you have more than these two files, you might want to organize them in a subdirectory (or more than one subdirectory). With the following directory structure:

```
.
├── myOtherSubdir
│   ├── fifthFile.nim
│   └── fourthFile.nim
├── mySubdir
│   └── thirdFile.nim
├── firstFile.nim
└── secondFile.nim
```

If you wanted to import all other files in your `secondFile.nim`, this is how you would do it:

```nim
import firstFile
import mySubdir/thirdFile
import myOtherSubdir / [fourthFile, fifthFile]
```

## Interacting with user input

Using the stuff we’ve introduced so far (basic data types and containers, control flow, loops) allows us to make quite a few simple programs. In this chapter we will learn how to make our programs more interactive. For that we need an option to read data from a file, or ask a user for an input.

### Reading from a file

Let’s say we have a text file called `people.txt` in the same directory as our Nim code. The contents of that file look like this:

#### `people.txt`

```
Alice A.
Bob B.
Carol C.
```

We want to use the contents of that file in our program, as a list (sequence) of names.

#### `readFromFile.nim`

```nim
import strutils

let contents = readFile("people.txt")
echo contents

let people = contents.splitLines()
echo people
```

- To read contents of a file, we use the `readFile` procedure, and we provide a path to the file from which to read (if the file is in the same directory as our Nim program, providing a filename is enough). The result is a multiline string.
- To split a multiline string into a sequence of strings (each string contains all the contents of a single line) we use `splitLines` from the `strutils` module.

Output:

```
Alice A.
Bob B.
Carol C.

@["Alice A.", "Bob B.", "Carol C.", ""]
```

There was a final new line (empty last line) in the original file, which is also present here. Because of the final new line, our sequence is longer than we expected/wanted. To solve the problem of a final new line, we can use the `strip` procedure from `strutils` after we have read from a file. All this does is remove any so-called whitespace from the start and end of our string. Whitespace is simply any character that makes some space, new-lines, spaces, tabs, etc.

#### `readFromFile2.nim`

```nim
import strutils

let contents = readFile("people.txt").strip()
echo contents

let people = contents.splitLines()
echo people
```

Using `strip` provides the expected results.

Output:

```
Alice A.
Bob B.
Carol C.

@["Alice A.", "Bob B.", "Carol C."]
```

### Reading user input

If we want to interact with a user, we must be able to ask them for an input, and then process it and use it. We need to read from standard input (stdin) by passing `stdin` to the `readLine` procedure.

#### `interaction1.nim`

```nim
echo "Please enter your name:"
let name = readLine(stdin)

echo "Hello ", name, ", nice to meet you!"
```

The type of `name` is inferred to be a string.

Output:

```
Please enter your name:
```

Waiting for user input. After we write our name and press Enter, the program will continue.

```
Please enter your name:
Alice
Hello Alice, nice to meet you!
```

### Dealing with numbers

Reading from a file or from user input always gives a string as a result. If we would like to use numbers, we need to convert strings to numbers: we again use the `strutils` module and use `parseInt` to convert to integers or `parseFloat` to convert into a float.

#### `interaction2.nim`

```nim
import strutils

echo "Please enter your year of birth:"
let yearOfBirth = readLine(stdin).parseInt()

let age = 2018 - yearOfBirth

echo "You are ", age, " years old."
```

Convert a string to an integer. When written like this, we trust our user to give a valid integer. What would happen if a user inputs '79 or ninety-three? Try it yourself.

Output:

```
Please enter your year of birth:
```

Waiting for user input. After entering the year of birth, the program will calculate the age.

```
Please enter your year of birth:
1934
You are 84 years old.
```

If we have a file `numbers.txt` in the same directory as our Nim code, with the following content:

#### `numbers.txt`

```
27.3
98.24
11.93
33.67
55.01
```

And we want to read that file and find the sum and average of the numbers provided, we can do something like this:

#### `interaction3.nim`

```nim
import strutils, sequtils, math

let
  strNums = readFile("numbers.txt").strip().splitLines()
  nums = strNums.map(parseFloat)

let
  sumNums = sum(nums)
  average = sumNums / float(nums.len)

echo sumNums
echo average
```

- We import multiple modules. `strutils` gives us `strip` and `splitLines`, `sequtils` gives `map`, and `math` gives `sum`.
- We strip the final new line, and split lines to create a sequence of strings.
- `map` works by applying a procedure (in this case `parseFloat`) to each member of a container. In other words, we convert each string to a float, returning a new sequence of floats.
- Using `sum` from `math` module to give us the sum of all elements in a sequence.
- We need to convert the length of a sequence to float because `sumNums` is a float.

Output:

```
226.15
45.23
```
