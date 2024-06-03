---
title: "Creating a Token With Huff"
description: "Huff is a domain-specific, low-level programming language designed explicitly for writing smart contracts on the Ethereum blockchain."
icon: "code"
draft: false
---

## Creating a Token in Huff

Creating a token on the Ethereum network, especially adhering to popular standards like ERC20, is a common task in smart contract development. In this section, we'll develop a basic ERC20 token using Huff. This example will demonstrate fundamental token functionalities such as transferring tokens and keeping track of balances.

### Overview of ERC20 Token Standard

The ERC20 standard defines a set of rules that an Ethereum token contract must follow. It includes methods for transferring tokens, fetching balances, and obtaining the total supply of tokens. Implementing these functionalities in Huff will provide a deeper understanding of the ERC20 standard and Huff's capabilities.

### Step-by-Step Guide to Creating an ERC20 Token in Huff

- **Token Initialization**: Set up the total supply and allocate it to the contract creator.
- **Transfer Functionality**: Implement a function to transfer tokens from one account to another.
- **Balance Tracking**: Maintain a mapping of account balances.

### Code Example: ERC20 Token Contract in Huff

```huff
// Define storage slots
#define constant TOTAL_SUPPLY_SLOT = 0x0
#define constant BALANCES_SLOT = 0x1

// Macros for common operations
#define macro MSTORE_BALANCE() = takes (2) returns (0) {
    // Store balance at the specified address
    dup2 0x1 add mstore
}

#define macro MLOAD_BALANCE() = takes (1) returns (1) {
    // Load balance for the specified address
    0x1 add mload
}

// Initialize the contract with total supply
#define macro INITIALIZE() = takes (0) returns (0) {
    // Set total supply
    1000000 TOTAL_SUPPLY_SLOT sstore
    // Allocate total supply to contract creator
    caller BALANCES_SLOT sstore
}

// Transfer tokens
#define macro TRANSFER() = takes (2) returns (0) {
    // Load sender balance
    caller MLOAD_BALANCE()
    // Check if sender has enough balance
    dup3 gt(assert_enough_balance)
    // Subtract amount from sender balance
    sub
    caller MSTORE_BALANCE()
    // Add amount to recipient balance
    over MLOAD_BALANCE()
    add
    swap MSTORE_BALANCE()
}

// Define label for assertion failure
#define macro assert_enough_balance() = takes (0) returns (0) {
    // Revert transaction if balance is insufficient
    // ...
}

// Main entry point
#define macro MAIN() = takes (0) returns (0) {
    INITIALIZE()
    // Additional contract logic...
}
```

In this Huff contract example, we define macros for initializing the token with a total supply and allocating it to the contract creator. The `TRANSFER` macro implements the logic for transferring tokens between accounts, checking balances, and updating them accordingly.

---

This document provides a comprehensive introduction to Huff, from setting up the development environment to writing and deploying smart contracts, including advanced features and practical examples like creating an ERC20 token.

