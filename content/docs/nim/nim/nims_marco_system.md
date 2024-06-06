---
title: "Nim Macros: A Comprehensive Tutorial"
description: "Nim Lang description"
icon: "code"
draft: false
---

Macros in Nim are powerful tools that allow you to transform Nim's abstract syntax tree (AST) at compile-time. This tutorial will guide you through the essential concepts and practical applications of Nim's macro system.

#### What is a Macro?
A macro in Nim is a function that executes at compile-time, transforming a Nim syntax tree into a different tree. This capability can be leveraged for various tasks, such as:
1. **Enhanced Assertions**: Implement an `assert` macro that prints both sides of a comparison if the assertion fails.
2. **Debugging**: Create a `debug` macro that prints the value and name of a symbol.
3. **Symbolic Differentiation**: Automatically perform differentiation on mathematical expressions.

#### Example Implementations

1. **Assertion Macro**:
    ```nim
    macro myAssert(arg: untyped): untyped =
      arg.expectKind nnkInfix
      arg.expectLen 3
      let op = newLit(" " & arg[0].repr & " ")
      let lhs = arg[1]
      let rhs = arg[2]
      
      result = quote do:
        if not `arg`:
          raise newException(AssertionDefect, $`lhs` & `op` & $`rhs`)

    let a = 1
    let b = 2
    myAssert(a != b)
    myAssert(a == b)
    ```

2. **Debug Macro**:
    ```nim
    macro myDebugEcho(arg: untyped): untyped =
      result = quote do:
        echo `arg.repr`, ": ", `arg`

    let x = 42
    myDebugEcho(x)
    ```

3. **Symbolic Differentiation**:
    ```nim
    import macros

    macro diff(expr: untyped, var: untyped): untyped =
      # Implementation of symbolic differentiation goes here
      # This example is a placeholder
      result = quote do:
        echo "Differentiation logic"
      
    let a = 2
    let b = 3
    diff(a * b, a)
    ```

#### Macro Arguments
Macro arguments can be typed or untyped, each with its advantages and limitations.

- **Untyped Arguments**: Passed before semantic checks, leading to simpler syntax trees but less flexibility with Nim's overload resolution.
- **Typed Arguments**: Passed after semantic checks, offering more complex trees with type information.

#### Static Arguments
Static arguments pass values directly to the macro, not as syntax trees.

```nim
import std/macros

macro myMacro(arg: static[int]): untyped =
  echo arg # Outputs the integer value directly

myMacro(1 + 2 * 3) # Outputs: 7
```

#### Code Blocks as Arguments
You can pass complex syntax trees to macros using indented code blocks.

```nim
echo "Hello ":
  let a = "Wor"
  let b = "ld!"
  a & b
```

This feature is particularly useful for macros, allowing the passage of complex trees.

#### Understanding the Syntax Tree
To build and manipulate Nim's syntax tree, it's crucial to understand how Nim source code is represented. The `macros.treeRepr` function is invaluable for this, as it converts a syntax tree into a readable string.

```nim
import macros

dumpTree:
  var mt: MyType = MyType(a:123.456, b:"abcdef")

# Output example:
# StmtList
#   VarSection
#     IdentDefs
#       Ident "mt"
#       Ident "MyType"
#       ObjConstr
#         Ident "MyType"
#         ExprColonExpr
#           Ident "a"
#           FloatLit 123.456
#         ExprColonExpr
#           Ident "b"
#           StrLit "abcdef"
```

#### Custom Semantic Checking
Before transforming the syntax tree, macros should verify the form of their arguments.

```nim
macro myAssert(arg: untyped): untyped =
  arg.expectKind nnkInfix
  arg.expectLen 3
  # Further processing...
```

#### Generating Code
You can generate code in two ways: 
1. **Using `newTree` and `newLit`**:
    ```nim
    import std/macros

    let tree = newTree(nnkStmtList, newLit(42))
    ```

2. **Using `quote do`**:
    ```nim
    import std/macros

    macro a(i) = quote do:
      let `i` = 0

    a b
    doAssert b == 0
    ```

#### Building Your First Macro
To create a macro like `myAssert`, start with a simple example and print the argument to understand its structure.

```nim
import std/macros

macro myAssert(arg: untyped): untyped =
  echo arg.treeRepr

let a = 1
let b = 2

myAssert(a != b)
```

#### Power and Responsibility
Macros are powerful and should be used judiciously. Prefer templates or generics when possible, and ensure macros are well-documented to maintain code clarity.

#### Limitations
Macros share the limitations of the NimVM, such as the inability to call non-compiler C functions directly.

By understanding these core concepts and techniques, you can harness the full power of Nim's macro system to write more expressive and efficient code.