---
title: "Containers and Procedures in Nim"
description: "Nim Lang description"
icon: "code"
draft: false
---

## Containers

Containers are data types which contain a collection of items and allow us to access those elements. Typically a container is also iterable, meaning that we can use them the same way we used strings in the loops chapter.

### Arrays
An array is the simplest container type.
The elements of an array are enclosed inside of square brackets.

```nim
var
  a: array[3, int] = [5, 7, 9]
  b = [5, 7, 9]        
  c = []  # error      
  d: array[7, string] 
```

If we provide the values, the length and type of array b are known at compile time. Although correct, there is no need to specifically declare it like array a.

Neither the length nor the type of the elements can be inferred from this kind of declaration — this produces an error.

The correct way to declare an empty array (which will be filled later) is to give its length and type, without providing the values of its elements — array d can contain seven strings.

### Sequences
Sequences are containers similar to arrays, but their length doesn’t have to be known at compile time, and it can change during runtime: we declare only the type of the contained elements with seq[<type>]. Sequences are also homogeneous, i.e. every element in a sequence has to be the same type.
The elements of a sequence are enclosed between @[ and ].
```nim
var
  e1: seq[int] = @[]   
  f = @["abc", "def"] 
```

The type of an empty sequence must be declared.

The type of a non-empty sequence can be inferred. In this case, it is a sequence containing strings.
Another way to initialize an empty sequence is to call the newSeq procedure.
```nim
var
  e = newSeq[int]()
```

Providing the type parameter inside of square brackets allows the procedure to know that it shall return a sequence of a certain type.
A frequent error is omission of the final (), which must be included.
We can add new elements to a sequence with the add function, similar to how we did with strings. For this to work the sequence must be mutable (defined with var), and the element we’re adding must be of the same type as the elements in the sequence.

seq.nim
```nim
var
  g = @['x', 'y']
  h = @['1', '2', '3']

g.add('z')  
echo g

h.add(g)    
echo h
```

Adding a new element of the same type (char).

Adding another sequence containing the same type.

```nim
@['x', 'y', 'z']
@['1', '2', '3', 'x', 'y', 'z']
# Trying to pass different types to the existing sequences will produce an error:
var i = @[9, 8, 7]

i.add(9.81) # error 
g.add(i)    # error
```

Trying to add a float to a sequence of int.

Trying to add a sequence of int to a sequence of char.
Since sequences can vary in length we need a way to get their length, for this we can use the len function.
```nim
var i = @[9, 8, 7]
echo i.len

i.add(6)
echo i.len

3
4
```
### Indexing and slicing
Indexing allows us to get a specific element from a container by its index. Think of the index as a position inside of the container.
Nim, like many other programming languages, has zero-based indexing, meaning that the first element in a container has the index zero, the second element has the index one, etc.
If we want to index "from the back", it is done by using the ^ prefix. The last element (first from the back) has index ^1.
The syntax for indexing is <container>[<index>].
indexing.nim
```nim
let j = ['a', 'b', 'c', 'd', 'e']

echo j[1]   
echo j[^1] 

b
e
```

Slicing allows us to get a series of elements with one call. It uses the same syntax as ranges.
If we use start .. stop syntax, both ends are included in the slice. Using start ..< stop syntax, the stop index is not included in the slice.
The syntax for slicing is <container>[<start> .. <stop>].

indexing.nim
```nim
echo j[0 .. 3]
echo j[0 ..< 3]
@[a, b, c, d]
@[a, b, c]
```
### Tuples

Both of the containers we’ve seen so far have been homogeneous. Tuples, on the other hand, contain heterogeneous data, i.e. elements of a tuple can be of different types. Similarly to arrays, tuples have fixed-size.
The elements of a tuple are enclosed inside of parentheses.
tuples.nim
```nim
let n = ("Banana", 2, 'c')  
echo n
```

Tuples can contain fields of different types. In this case: string, int, and char.
(Field0: "Banana", Field1: 2, Field2: 'c')
We can also name each field in a tuple to distinguish them. This can be used for accessing the elements of the tuple, instead of indexing.
tuples.nim
```nim
var o = (name: "Banana", weight: 2, rating: 'c')

o[1] = 7          
o.name = "Apple"  
echo o
```

Changing the value of a field by using the field’s index.

Changing the value of a field by using the field’s name.

`(name: "Apple", weight: 7, rating: 'c')`

## Procedures

Procedures, or functions as they are called in some other programming languages, are parts of code that perform a specific task, packaged as a unit. The benefit of grouping code together like this is that we can call these procedures instead of writing all the code over again when we wish to use the procedure’s code.

### Declaring a procedure
Before we can use (call) our procedure, we need to create it and define what it does.
A procedure is declared by using the proc keyword and the procedure name, followed by the input parameters and their type inside of parentheses, and the last part is a colon and the type of the value returned from a procedure, like this:
`proc <name>(<p1>: <type1>, <p2>: <type2>, ...): <returnType>`
The body of a procedure is written in the indented block following the declaration appended with a = sign.

callProcs.nim
```nim
proc findMax(x: int, y: int): int =  
  if x > y:
    return x  
  else:
    return y
  # this is inside of the procedure
# this is outside of the procedure
```

Declaring procedure called findMax, which has two parameters, x and y, and it returns an int type.

To return a value from a procedure, we use the return keyword.
 
```nim 
proc echoLanguageRating(language: string) = 
  case language
  of "Nim", "nim", "NIM":
    echo language, " is the best language!"
  else:
    echo language, " might be a second-best language."
```

The echoLanguageRating procedure just echoes the given name, it doesn’t return anything, so the return type is not declared.
 
Normally we’re not allowed to change any of the parameters we are given. Doing something like this will throw an error:
```nim
proc changeArgument(argument: int) =
  argument += 5

var ourVariable = 10
changeArgument(ourVariable)
```
In order for this to work we need to allow Nim, and the programmer using our procedure, to change the argument by declaring it as a variable:
```nim
proc changeArgument(argument: var int) = 
  argument += 5

var ourVariable = 10
changeArgument(ourVariable)
echo ourVariable
changeArgument(ourVariable)
echo ourVariable
```

Notice how argument is now declared as a var int and not just as an int.

15
20

This of course means that the name we pass it must be declared as a variable as well, passing in something assigned with const or let will throw an error.
While it is good practice to pass things as arguments it is also possible to use names declared outside the procedure, both variables and constants:
```nim
var x = 100

proc echoX() =
  echo x  
  x += 1  

echoX()
echoX()
```

Here we access the outside variable x.

We can also update its value, since it’s declared as a variable.

100
101


### Calling the procedures
After we have declared a procedure, we can call it. The usual way of calling procedures/functions in many programming languages is to state its name and provide the arguments in the parentheses, like this:
`<procName>(<arg1>, <arg2>, ...)`

The result from calling a procedure can be stored in a variable.
If we want to call our findMax procedure from the above example, and save the return value in a variable we can do that with:
callProcs.nim

```nim
let
  a = findMax(987, 789)
  b = findMax(123, 321)
  c = findMax(a, b)  
echo a
echo b
echo c
```

The result from the function findMax is here named c, and is called with the results of our first two calls (findMax(987, 321)).

987
321
987
 
Nim, unlike many other languages, also supports Uniform Function Call Syntax, which allows many different ways of calling procedures.
This one is a call where the first argument is written before the function name, and the rest of the parameters are stated in parentheses:
<arg1>.<procName>(<arg2>, ...)
We have used this syntax when we were adding elements to an existing sequence (<seq>.add(<element>)), as this makes it more readable and expresses our intent more clearly than writing add(<seq>, <element>). We can also omit the parentheses around the arguments:
<procName> <arg1>, <arg2>, ...
We’ve seen this style being used when we call the echo procedure, and when calling the len procedure without any arguments. These two can also be combined like this, but this syntax however is not seen very often:
<arg1>.<procName> <arg2>, <arg3>, ...
 
The uniform call syntax allows for more readable chaining of multiple procedures:
```nim
ufcs.nim
proc plus(x, y: int): int =  
  return x + y

proc multi(x, y: int): int =
  return x * y

let
  a = 2
  b = 3
  c = 4

echo a.plus(b) == plus(a, b)
echo c.multi(a) == multi(c, a)


echo a.plus(b).multi(c)  
echo c.multi(b).plus(a) 
```

If multiple parameters are of the same type, we can declare their type in this compact way.

First we add a and b, then the result of that operation (2 + 3 = 5) is passed as the first parameter to the multi procedure, where it is multiplied by c (5 * 4 = 20).

First we multiply c and b, then the result of that operation (4 * 3 = 12) is passed as the first parameter to the plus procedure, where it is added with a (12 + 2 = 14).

true
true
20
14

### Forward declaration
As mentioned in the very beginning of this section we can declare a procedure without a code block. The reason for this is that we have to declare procedures before we can call them, doing this will not work:
echo 5.plus(10) # error      
```nim
proc plus(x, y: int): int =  
  return x + y
```

This will throw an error as plus isn’t defined yet.

Here we define plus, but since it’s after we use it Nim doesn’t know about it yet.
The way to get around this is what’s called a forward declaration:
```nim
proc plus(x, y: int): int    

echo 5.plus(10)              

proc plus(x, y: int): int =  
  return x + y
```

Here we tell Nim that it should consider the plus procedure to exist with this definition.

Now we are free to use it in our code, this will work.

This is were plus is actually implemented, this must of course match our previous definition.
