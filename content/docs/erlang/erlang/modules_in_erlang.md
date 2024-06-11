---
title: "Modules in Erlang Programming Language"
description: ""
icon: "code"
draft: false
---

Modules in Erlang are collections of functions grouped within a single file under a unified name. In Erlang, all functions must be defined within modules.

Many fundamental functionalities, such as arithmetic, logic, and Boolean operations, are readily available as default modules loaded upon program execution. However, any other function defined within a module must be invoked in the format: `Module:Function(Arguments)`.

### Defining a Module

Modules can declare two primary entities: functions and attributes. Attributes serve as metadata describing the module itself, including its name, externally visible functions, authorship details, and more. This metadata aids the compiler in its tasks and enables users to retrieve essential information from compiled code without needing to consult the source.

The syntax for declaring a function within a module is as follows:

```erlang
-module(modulename).
```

Here, `modulename` represents the name of the module and must be the first line of code within the module.

Consider the following example of a module named `helloworld`:

```erlang
-module(helloworld).
-export([start/0]).

start() -> 
   io:fwrite("Hello World").
```

The output of this program is:

```
Hello World
```

### Module Attributes

A module attribute defines specific properties of a module and comprises a tag and a corresponding value.

The general syntax for an attribute is:

```erlang
-Tag(Value).
```

Below is an example demonstrating the usage of attributes:

```erlang
-module(helloworld).
-author("TutorialPoint").
-version("1.0").
-export([start/0]).

start() -> 
   io:fwrite("Hello World").
```

In this example, two custom attributes, `author` and `version`, are defined, containing the program's author and version number, respectively.

The output of the program remains:

```
Hello World
```

### Pre-built Attributes

Erlang provides pre-built attributes that can be attached to modules. Let's explore them:

#### Export

The `export` attribute specifies a list of functions and their arity to be exported for use by other modules, thereby defining the module's interface.

```erlang
-export([FunctionName1/FunctionArity1,...,FunctionNameN/FunctionArityN]).
```

Consider the following example:

```erlang
-module(helloworld).
-author("TutorialPoint").
-version("1.0").
-export([start/0]).

start() -> 
   io:fwrite("Hello World").
```

The output of this program is unchanged:

```
Hello World
```

#### Import

The `import` attribute enables the importation of functions from another module for local use.

```erlang
-import(modulename, [functionname/parameter]).
```

In the following example, the `io` module and its `fwrite` function are imported:

```erlang
-module(helloworld).
-import(io, [fwrite/1]).
-export([start/0]).

start() -> 
   fwrite("Hello, world!\n").
```

With this importation, mentioning the `io` module name is unnecessary whenever invoking the `fwrite` function.

The output of this program is:

```
Hello, world!
```

These attributes serve to enhance the modularity and flexibility of Erlang programs, facilitating clear organization and reuse of code components.