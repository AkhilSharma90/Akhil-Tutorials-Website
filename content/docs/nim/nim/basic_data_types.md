---
title: "Basic data types in Nim"
description: "Learn about basic data types using the nim language."
icon: "code"
draft: false
---

In this section, we will be learning more about basic data types in Nim.

<!-- Basic data types -->

## Integers

Create a file called `Integers.nim` and have the following code:

```nim
let
  a = 11
  b = 4

echo "a + b = ", a + b
echo "a - b = ", a - b
echo "a * b = ", a * b
echo "a / b = ", a / b
echo "a div b = ", a div b
echo "a mod b = ", a mod b
```

The echo command will print to the screen everything that follows it separated by commas. In this case, it first prints the string a + b = , and then after it, in the same row, it prints the result of the expression a + b.
We can compile and run the above code, and the output should be:

```nim
a + b = 15
a - b = 7
a * b = 44
a / b = 2.75
a div b = 2
a mod b = 3
```

## Floats

Create a file called `floats.nim` and have the following code

```nim
let
  c = 6.75
  d = 2.25

echo "c + d = ", c + d
echo "c - d = ", c - d
echo "c * d = ", c * d
echo "c / d = ", c / d
c + d = 9.0
c - d = 4.5
c * d = 15.1875
c / d = 3.0
```

Notice that in the addition and division examples, even though we get a number without a decimal part, the result is still of the floating type.
The precedence of mathematical operations is as one would expect: multiplication and division have higher priority than addition and subtraction.

```nim
echo 2 + 3 * 4
echo 24 - 8 / 4
14
22.0
```

## Converting floats and integers

Mathematical operations between variables of different numerical types are not possible in Nim, and they will produce an error:

```nim
let
  e = 5
  f = 23.456

echo e + f   # error
```

The values of variables need to be converted to the same type. Conversion is straight-forward: to convert to an integer, we use the int function, and to convert to a float the float function is used.

```nim
let
  e = 5
  f = 23.987

echo float(e)
echo int(f)

echo float(e) + f
echo e + int(f)

# output
5.0
23
28.987
28

```

## Characters

The char type is used for representing a single ASCII character.
Chars are written between two single ticks ('). Chars can be letters, symbols, or single digits. Multiple digits or multiple letters produce an error.

```nim
let
  h = 'z'
  i = '+'
  j = '2'
  k = '35' # error
  l = 'xy' # error

```

## Strings

Strings can be described as a series of characters. Their content is written between two double quotes (").

strings.nim

```nim
let
  m = "word"
  n = "A sentence with interpunction."
  o = ""
  p = "32"
  q = "!"

```

**String concatenation**

Strings in Nim are mutable, meaning their content can change. With the add function we can add (append) either another string or a char to an existing string. If we don’t want to change the original string, we can also concatenate (join together) strings with the & operator, this returns a new string.

stringConcat.nim

```nim
var
  p = "abc"
  q = "xy"
  r = 'z'

p.add("def")
echo "p is now: ", p

q.add(r)
echo "q is now: ", q

echo "concat: ", p & q

echo "p is still: ", p
echo "q is still: ", q
```

```bash
p is now: abcdef
q is now: xyz
concat: abcdefxyz
p is still: abcdef
q is still: xyz
```

**Relational operators**

Relational operators test the relation between two entities, which must be comparable.
To compare if two values are the same, == (two equal signs) is used. Do not confuse this with =, which is used for assignment as we saw earlier.
Here are all the relational operators defined for integers:

relationalOperators.nim

```nim
let
  g = 31
  h = 99

echo "g is greater than h: ", g > h
echo "g is smaller than h: ", g < h
echo "g is equal to h: ", g == h
echo "g is not equal to h: ", g != h
echo "g is greater or equal to h: ", g >= h
echo "g is smaller or equal to h: ", g <= h
g is greater than h: false
g is smaller than h: true
g is equal to h: false
g is not equal to h: true
g is greater or equal to h: false
g is smaller or equal to h: true
```

We can also compare characters and strings:
relationalOperators.nim

```nim
let
  i = 'a'
  j = 'd'
  k = 'Z'

echo i < j
echo i < k

let
  m = "axyb"
  n = "axyz"
  o = "ba"
  p = "ba "

echo m < n
echo n < o
echo o < p
```

```bash
true
false
true
true
true
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
