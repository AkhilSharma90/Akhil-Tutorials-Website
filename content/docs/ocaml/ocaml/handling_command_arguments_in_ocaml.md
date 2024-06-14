---
title: "Handling Command-Line Arguments in OCaml"
description: "Learn how to handle command line arguments in ocaml"
icon: "code"
draft: false
---


In this tutorial, we will learn how to read command-line arguments directly using OCaml's `Sys.argv` array and then how to simplify the process using the standard library's `Arg` module.

## Using `Sys.argv`

Like in C and many other languages, the arguments passed to a program on the command line are stored in an array named `argv`. This array is found in the `Sys` module of the standard library and is referred to as `Sys.argv`. The number of arguments, including the program's name, is obtained using the `Array.length` function.

### Example: Displaying Command-Line Arguments

The following program displays the arguments along with their position in `Sys.argv`:

```ocaml
let () =
  for i = 0 to Array.length Sys.argv - 1 do
    Printf.printf "[%i] %s\n" i Sys.argv.(i)
  done
```

Save the above code as `args.ml`, and run it as follows:

```sh
$ ocaml args.ml arg1 arg2 arg3
[0] args.ml
[1] arg1
[2] arg2
[3] arg3
```

Alternatively, you can compile your program and run it:

```sh
$ ocamlopt -o args args.ml
$ ./args arg1 arg2 arg3
[0] ./args
[1] arg1
[2] arg2
[3] arg3
```

## Using the `Arg` Module

The OCaml standard library includes the `Arg` module for handling command-line interfaces, which simplifies the process compared to using `Sys.argv` directly.

### Example: Appending Files

We'll consider an example from the OCaml documentation: a program for appending files.

#### Step 1: Define Usage Message

Set up the usage message to be printed in case of a malformed command line or when help is requested:

```ocaml
let usage_msg = "append [-verbose] <file1> [<file2>] ... -o <output>"
```

#### Step 2: Create References

Create references to hold the information gathered from the command line. The `Arg` module will fill these in as the command line is read.

```ocaml
let verbose = ref false
let input_files = ref []
let output_file = ref ""
```

#### Step 3: Handle Anonymous Inputs

Define a function to handle the anonymous inputs (those with no flag). These will be our input file names.

```ocaml
let anon_fun filename = input_files := filename :: !input_files
```

#### Step 4: Define Command-Line Specifications

Build the list of command-line flag specifications. Each is a tuple of the flag name, the action to be taken when encountered, and the help string.

```ocaml
let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]
```

#### Step 5: Parse Command-Line Arguments

Call `Arg.parse`, giving it the specification list, anonymous function, and usage message. Once it returns, the references will contain the required information.

```ocaml
let () = Arg.parse speclist anon_fun usage_msg

(* Main functionality here *)
```

#### Complete Program

Here is the entire program:

```ocaml
let usage_msg = "append [-verbose] <file1> [<file2>] ... -o <output>"

let verbose = ref false
let input_files = ref []
let output_file = ref ""

let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  (* Main functionality here *)
```

### Compilation and Execution

Save the program as `append.ml`, compile it, and try it out:

```sh
$ ocamlopt -o append append.ml
$ ./append -verbose one.txt two.txt -o three.txt
$ ./append one.txt two.txt
$ ./append -quiet
./append: unknown option '-quiet'.
append [-verbose] <file1> [<file2>] ... -o <output>
  -verbose Output debug information
  -o Set output file name
  -help  Display this list of options
  --help  Display this list of options
$ ./append -help
append [-verbose] <file1> [<file2>] ... -o <output>
  -verbose Output debug information
  -o Set output file name
  -help  Display this list of options
  --help  Display this list of options
```

### Other Tools for Parsing Command-Line Options

There are libraries with more extensive facilities than the built-in `Arg` module:

- **Cmdliner**: A modern interface for command line processing, which also generates UNIX man pages automatically.
- **Clap**: An imperative command line parser.
- **Minicli**: Supports rejecting malformed command lines that others might silently accept.
- **Getopt for OCaml**: Similar to GNU `getopt`.

By using these tools, you can handle complex command-line interfaces efficiently in OCaml.