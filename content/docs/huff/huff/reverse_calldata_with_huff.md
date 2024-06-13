---
title: "Reversing Calldata with Huff: A Step-by-Step Tutorial"
description: "Huff is a domain-specific, low-level programming language designed explicitly for writing smart contracts on the Ethereum blockchain."
icon: "code"
draft: false
---

## Introduction

In this tutorial, we'll explore the process of writing a Huff smart contract that reverses the calldata it receives. Calldata is a type of input data sent along with a transaction, stored outside the EVM's storage and memory, making it cheaper to use.

Our goal is to create a contract that, upon receiving data, returns the same data in reverse order.

The task is to write a Huff smart contract that reverses the calldata it receives. When data is sent to this contract, it should return the same data but in reverse order.

## Solution

There are multiple valid solutions to this challenge.

```huff
#define constant NEG1 = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

#define macro GET_CALLDATA_BYTE() = takes(1) returns(1) {
  calldataload 0xf8 shr
}

#define macro MAIN() = takes(0) returns(0) {
  calldatasize not_empty jumpi
  returndatasize returndatasize return
  
  not_empty:
  calldatasize
  returndatasize

  copy_bytes_iter:           // [i, j + 1]
    swap1                    // [j + 1, i]
    [NEG1] add               // [j, i]
    dup2 dup2                // [j, i, j, i]
    dup2 GET_CALLDATA_BYTE() // [cd[i], j, i, j, i]
    dup2 GET_CALLDATA_BYTE() // [cd[j], cd[i], j, i, j, i]
    swap2                    // [j, cd[i], cd[j], i, j, i]
    mstore8                  // [cd[j], i, j, i]
    swap1 mstore8            // [j, i]
    swap1 0x1 add            // [i', j' + 1]
    dup2 dup2                // [i', j' + 1, i', j' + 1]
    lt 
    copy_bytes_iter jumpi

  calldatasize returndatasize return
}
```

## Breakdown of the Contract

Let's break down the solution into manageable parts.

### Constant Definition

Firstly, we define a constant `NEG1`, which is a 256-bit number representing -1 in two's complement form. This constant will be useful for offsetting indexes.

```huff
#define constant NEG1 = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
```

### Calldata Byte Retrieval Macro

Next, we define a macro called `GET_CALLDATA_BYTE()`. This macro fetches one byte of calldata at a specified index. The `calldataload` opcode loads 32 bytes of calldata from a specific index, but we're only interested in a single byte, so we shift right (`shr`) by 248 bits (0xf8) to isolate the byte we need.

```huff
#define macro GET_CALLDATA_BYTE() = takes(1) returns(1) {
  calldataload 0xf8 shr
}
```

### Main Macro

Next comes the `MAIN()` macro. The first part of `MAIN()` checks whether any calldata is present:

```huff
#define macro MAIN() = takes(0) returns(0) {
  calldatasize not_empty jumpi
  returndatasize returndatasize return
```

Here, `calldatasize` gets the size of the calldata. If the size is non-zero (meaning calldata is present), control jumps to the `not_empty` label. If the size is zero (no calldata), it immediately returns.

After confirming that calldata is present, the size of the calldata is fetched and pushed to the stack.

```huff
not_empty:
  calldatasize
  returndatasize
```

### Reversing Calldata Logic

The most intricate part is the logic to reverse the calldata, one byte at a time. Let's divide the `copy_bytes_iter` block into smaller chunks and discuss each one.

#### Block 1: Index Preparation and Byte Retrieval

```huff
copy_bytes_iter:           // [i, j + 1]
  swap1                    // [j + 1, i]
  [NEG1] add               // [j, i]
  dup2 dup2                // [j, i, j, i]
  dup2 GET_CALLDATA_BYTE() // [cd[i], j, i, j, i]
  dup2 GET_CALLDATA_BYTE() // [cd[j], cd[i], j, i, j, i]
```

In this first block, we prepare the indices and retrieve the bytes to be swapped. We first swap `i` and `j + 1` then subtract 1 from `j + 1` to get `j`. After duplicating `j` and `i` for later use, the `GET_CALLDATA_BYTE()` macro is invoked twice to get the `i`th and `j`th bytes (cd[i] and cd[j]) from the calldata.

#### Block 2: Byte Swapping

```huff
swap2                    // [j, cd[i], cd[j], i, j, i]
mstore8                  // [cd[j], i, j, i]
swap1 mstore8            // [j, i]
```

In the second block, the swapping of the bytes takes place. The contract swaps `cd[i]` and `j`, then uses `mstore8` to store `cd[j]` at the `i`th position and `cd[i]` at the `j`th position. At the end of this block, `j` and `i` are left on the stack.

#### Block 3: Loop Iteration and Continuation

```huff
swap1 0x1 add            // [i', j' + 1]
dup2 dup2                // [i', j' + 1, i', j' + 1]
lt 
copy_bytes_iter jumpi
```

In the third block, `i` is incremented to move on to the next byte from the start. The indices `i'` and `j' + 1` are then duplicated for comparison. If `i'` is less than `j' + 1`, the loop continues and jumps back to `copy_bytes_iter`. Otherwise, the loop terminates, and the contract proceeds to the next stage.

By repeating these steps, the `copy_bytes_iter` block swaps all pairs of bytes in the calldata until all bytes are reversed.

### Final Return

After completing the reversal of calldata, the contract returns the reversed data:

```huff
calldatasize returndatasize return
```

This completes the reversal process and outputs the reversed calldata.

## Conclusion

In this tutorial, we've seen how to write a Huff smart contract that reverses calldata. By breaking down the problem and implementing the solution step by step, we successfully created a contract that meets the challenge requirements. This approach can be adapted and expanded to handle more complex data manipulation tasks within the EVM using Huff.