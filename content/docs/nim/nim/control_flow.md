---
title: "Control flow in Nim"
description: "Part of our programs require control flow and we are going to be looking into that concept in this section."
icon: "code"
draft: false
---

So far in our programs every line of code was executed at some point. Control flow statements allow us to have parts of code which will be executed only if some boolean condition is satisfied.

## If statement

```nim
if <condition>:
  <indented block>
```

If statements can be nested, i.e. inside one if-block there can be another if statement.

if.nim

```nim
let
  a = 11
  b = 22
  c = 999

if a < b:
  echo "a is smaller than b"
  if 10*a < b:
    echo "not only that, a is *much* smaller than b"

if b < c:
  echo "b is smaller than c"
  if 10*b < c:
    echo "not only that, b is *much* smaller than c"

if a+b > c:
  echo "a and b are larger than c"
  if 1 < 100 and 321 > 123:
    echo "did you know that 1 is smaller than 100?"
    echo "and 321 is larger than 123! wow!"
```

```bash
a is smaller than b
b is smaller than c
not only that, b is *much* smaller than c
```

## Else

Else follows after an if-block and allows us to have a branch of code which will be executed when the condition in the if statement is not true.

else.nim

```nim
let
  d = 63
  e = 2.718

if d < 10:
  echo "d is a small number"
else:
  echo "d is a large number"

if e < 10:
  echo "e is a small number"
else:
  echo "e is a large number"

# d is a large number
# e is a small number
```

## Elif

Elif is short for "else if", and enables us to chain multiple if statements together.
The program tests every statement until it finds one which is true. After that, all further statements are ignored.

elif.nim

```nim
let
  f = 3456
  g = 7

if f < 10:
  echo "f is smaller than 10"
elif f < 100:
  echo "f is between 10 and 100"
elif f < 1000:
  echo "f is between 100 and 1000"
else:
  echo "f is larger than 1000"

if g < 1000:
  echo "g is smaller than 1000"
elif g < 100:
  echo "g is smaller than 100"
elif g < 10:
  echo "g is smaller than 10"

# f is larger than 1000
# g is smaller than 1000
```

## Case

A case statement is another way to only choose one of multiple possible paths, similar to the if statement with multiple elifs. A case statement, however, doesn’t take multiple boolean conditions, but rather any value with distinct states and a path for each possible value.
Code written with in if-elif block looking like this:

```nim
if x == 5:
  echo "Five!"
elif x == 7:
  echo "Seven!"
elif x == 10:
  echo "Ten!"
else:
  echo "unknown number"
can be written with case statement like this:
case x
of 5:
  echo "Five!"
of 7:
  echo "Seven!"
of 10:
  echo "Ten!"
else:
  echo "unknown number"
```

Unlike the if statement, case statement must cover all possible cases. If one is not interested in some of those cases, else: discard can be used.

```nim
let h = 'y'

case h
of 'x':
  echo "You've chosen x"
of 'y':
  echo "You've chosen y"
of 'z':
  echo "You've chosen z"
else: discard
```

Even though we are interested in only three values of h, we must include this line to cover all other possible cases (all other characters). Without it, the code would not compile.

## Loops

Loops are another control flow construct which allow us to run some parts of code multiple times.
In this chapter we will meet two kinds of loops:
• for-loop: run a known number of times
• while-loop: run as long some condition is satisfied
For loop
Syntax of a for-loop is:

```nim
for <loopVariable> in <iterable>:
  <loop body>
```

Traditionally, i is often used as a loopVariable name, but any other name can be used. That variable will be available only inside the loop. Once the loop has finished, the value of the variable is discarded.

The iterable is any object we can iterate through. Of the types already mentioned, strings are iterable objects.
All lines in the loop body are executed at every loop, which allows us to efficiently write repeating parts of code.

If we want to iterate through a range of (integer) numbers in Nim, the syntax for the iterable is start .. finish where start and finish are numbers. This will iterate through all the numbers between start and finish, including both start and finish. For the default range iterable, start needs to be smaller than finish.
If we want to iterate until a number (not including it), we can use ..<:

for1.nim

```nim
for n in 5 .. 9:
  echo n

echo ""

for n in 5 ..< 9:
  echo n
```

```bash
5
6
7
8
9

5
6
7
8
```

If we want to iterate through a range of numbers with a step size different than one, countup is used. With countup we define the starting value, the stopping value (included in the range), and the step size.

for2.nim

```nim
for n in countup(0, 16, 4):
  echo n
```

Counting up from zero to 16, with a step size of 4. The end (16) is included in the range.

```bash
0
4
8
12
16
```

To iterate through a range of numbers where the start is larger than finish, a similar function called countdown is used. Even if we’re counting down, the step size must be positive.

for2.nim

```nim
for n in countdown(4, 0):
  echo n

echo ""

for n in countdown(-3, -9, 2):
  echo n
```

```bash
4
3
2
1
0

-3
-5
-7
-9
```

Since string is an iterable, we can use a for-loop to iterate through each character of the string (this kind of iteration is sometimes called a for-each loop).

for3.nim

```nim
let word = "alphabet"

for letter in word:
  echo letter
a
l
p
h
a
b
e
t
```

If we also need to have an iteration counter (starting from zero), we can achieve that by using for <counterVariable>, <loopVariable> in <iterator>: syntax. This is very practical if you want to iterate through one iterable, and simultaneously access another iterable at the same offset.

for3.nim

```nim
for i, letter in word:
  echo "letter ", i, " is: ", letter

letter 0 is: a
letter 1 is: l
letter 2 is: p
letter 3 is: h
letter 4 is: a
letter 5 is: b
letter 6 is: e
letter 7 is: t
```

## While loop

While loops are similar to if statements, but they keep executing their block of code as long as the condition remains true. They are used when we don’t know in advance how many times the loop will run.
We must make sure the loop will terminate at some point and not become an infinite loop.
while.nim

```nim
var a = 1

while a*a < 10:
  echo "a is: ", a
  inc a

echo "final value of a: ", a
```

This condition will be checked every time before entering the new loop and executing the code inside of it.

inc is used to increment a by one. It is the same as writing a = a + 1 or a += 1.

```bash
a is: 1
a is: 2
a is: 3
final value of a: 4
```

## Break and continue

The break statement is used to prematurely exit from a loop, usually if some condition is met.
In the next example, if there were no if statement with break in it, the loop would continue to run and print until i becomes 1000. With the break statement, when i becomes 3, we immediately exit the loop (before printing the value of i).
break.nim

```nim
var i = 1

while i < 1000:
  if i == 3:
    break
  echo i
  inc i
```

```bash
1
2
```

The continue statement starts the next iteration of a loop immediately, without executing the remaining lines of the current iteration. Notice how 3 and 6 are missing from the output of the following code:
continue.nim

```nim
for i in 1 .. 8:
  if (i == 3) or (i == 6):
    continue
  echo i
```

```bash
1
2
4
5
7
8
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
