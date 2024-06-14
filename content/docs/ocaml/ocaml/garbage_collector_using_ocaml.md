---
title: "Working with the Garbage Collector in OCaml"
description: "Learn how to handle command line arguments in ocaml"
icon: "code"
draft: false
---

## Introduction

In this tutorial, we will explore how to use OCaml's `Gc` module to interact with the garbage collector and how to write your own finalizers. We will also provide exercises to help you develop a better understanding.

## The Gc Module

The `Gc` module contains useful functions for querying and controlling the garbage collector from OCaml programs.

### Example: Running GC and Printing Statistics

The following program runs and then prints out GC statistics before quitting:

```ocaml
let rec iterate r x_init i =
  if i = 1 then x_init
  else
    let x = iterate r x_init (i - 1) in
    r *. x *. (1.0 -. x)

let () =
  Random.self_init ();
  Graphics.open_graph " 640x480";
  for x = 0 to 640 do
    let r = 4.0 *. float_of_int x /. 640.0 in
    for i = 0 to 39 do
      let x_init = Random.float 1.0 in
      let x_final = iterate r x_init 500 in
      let y = int_of_float (x_final *. 480.) in
      Graphics.plot x y
    done
  done;
  Gc.print_stat stdout
```

When you run this program, it prints out GC statistics such as `minor_words`, `major_words`, `minor_collections`, and `major_collections`.

### Enabling GC Debugging Messages

To print debugging messages when certain events occur (e.g., on every major collection), add the following code at the beginning of the program:

```ocaml
Gc.set { (Gc.get ()) with Gc.verbose = 0x01 }
```

This will cause the GC to print a message at the start of every major collection.

## Finalization and the Weak Module

You can write a finalizer function that gets called when an object is about to be freed by the GC.

The `Weak` module lets you create weak pointers, which hint to the garbage collector that the object may be collected at any time.

### Example: In-Memory Object Database Cache

Let's create an in-memory object database cache that uses finalizers and weak pointers.

#### Data Structure

Define the in-memory and on-disk formats:

```ocaml
(* In-memory format. *)
type record = { mutable name : string; mutable address : string }

(* On-disk format. *)
let record_size = 256
let name_size = 64
let addr_size = 192
```

#### Low-Level Functions

Define functions to read, write, lock, and unlock records:

```ocaml
let seek_record n fd = ignore (Unix.lseek fd (n * record_size) Unix.SEEK_SET)

let write_record record n fd =
  seek_record n fd;
  ignore (Unix.write fd (Bytes.of_string record.name) 0 name_size);
  ignore (Unix.write fd (Bytes.of_string record.address) 0 addr_size)

let read_record record n fd =
  seek_record n fd;
  ignore (Unix.read fd (Bytes.of_string record.name) 0 name_size);
  ignore (Unix.read fd (Bytes.of_string record.address) 0 addr_size)

let lock_record n fd =
  seek_record n fd;
  Unix.lockf fd Unix.F_LOCK record_size

let unlock_record n fd =
  seek_record n fd;
  Unix.lockf fd Unix.F_ULOCK record_size
```

#### Creating New Records

Define a function to create new, empty in-memory record objects:

```ocaml
let new_record () =
  { name = String.make name_size ' '; address = String.make addr_size ' ' }
```

#### Fixed Number of Records

Set the total number of records:

```ocaml
let nr_records = 10000
let diskfile = Unix.openfile "users.bin" [ Unix.O_RDWR; Unix.O_CREAT ] 0o666
```

#### Record Cache

Define the cache of records:

```ocaml
let cache = Weak.create nr_records
```

#### Finalizer Function

Define the finalizer function:

```ocaml
let finaliser n record =
  Printf.printf "*** objcache: finalising record %d\n%!" n;
  write_record record n diskfile;
  unlock_record n diskfile
```

#### Get Record Function

Define the function to get a record from the cache or disk:

```ocaml
let get_record n =
  match Weak.get cache n with
  | Some record ->
      Printf.printf "*** objcache: fetching record %d from memory cache\n%!" n;
      record
  | None ->
      Printf.printf "*** objcache: loading record %d from disk\n%!" n;
      let record = new_record () in
      Gc.finalise (finaliser n) record;
      lock_record n diskfile;
      read_record record n diskfile;
      Weak.set cache n (Some record);
      record
```

#### Sync Records Function

Define the function to synchronize records:

```ocaml
let sync_records () =
  for i = 0 to nr_records - 1 do
    Weak.set cache i None
  done;
  Gc.full_major ()
```

#### Test Code

Download the complete program and test code `objcache.ml`, and compile it with:

```sh
$ ocamlc unix.cma objcache.ml -o objcache
```

## Conclusion

In this tutorial, we learned how to use the `Gc` module to interact with the garbage collector and how to write finalizers using the `Weak` module. We also implemented an in-memory object database cache as a practical example. By understanding and utilizing these concepts, you can manage memory more effectively in OCaml programs.