---
title: "Hello World using Huff"
description: "Huff is a domain-specific, low-level programming language designed explicitly for writing smart contracts on the Ethereum blockchain."
icon: "code"
draft: false
---

## Writing Your First Huff Contract

### Structure of a Huff Contract

Huff contracts, at their core, are a series of EVM opcodes and macros. The structure is significantly more straightforward than a typical Solidity contract. A basic Huff contract typically contains:

1. **Declarations**: Declaring macros and data storage locations.
2. **Macros**: Macros are reusable code blocks in Huff.
3. **Main Execution Logic**: The primary logic of the contract, often written using a combination of EVM opcodes and macros.

### Basic Syntax and Commands

Huff syntax is minimalistic, focusing on direct manipulation of the EVM stack. Here are some key components:

- **Macros**: Defined using `#define`, macros are a powerful feature for code reusability.
- **Storage and Memory Operations**: Directly manipulate storage and memory with EVM opcodes.
- **Control Flow**: Utilize opcodes like `JUMP`, `JUMPI`, `PUSH`, etc., for control flow.

### Code Example: A Simple "Hello World" Contract

Let's create a simple contract in Huff that stores a value and allows it to be retrieved.

```huff
#define macro MAIN() = takes (0) returns (0) {
    // Store the value 123 at storage location 0
    123 0 sstore
    // Retrieve and return the value from storage location 0
    0 sload
    mstore
    32 0 return
}

// The entry point of the contract
#define macro JUMPDEST_MAIN() = takes (0) returns (0) {
    MAIN()
}
```

This contract stores the number 123 in the first storage slot and retrieves it when called. The macros `MAIN` and `JUMPDEST_MAIN` encapsulate the contract's logic.

### Compiling and Deploying the Contract

After writing the contract, use the Huff compiler to compile it:

```sh
huffc your_contract.huff --bytecode
```

Then, deploy the compiled bytecode to the Ethereum network using Truffle or Hardhat.
