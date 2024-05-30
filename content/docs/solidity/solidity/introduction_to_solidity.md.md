---
title: "An Introduction To Solidity"
description: "Learn about Solidity, basics, language syntax and more."
icon: "api"
draft: false
---

### What is Solidity?

Solidity is an object-oriented programming language influenced by C++, JavaScript, and Python. It is designed to be compiled into bytecode that runs on the Ethereum Virtual Machine (EVM), which is the runtime environment for Solidity code, similar to how a browser runs JavaScript code.

In essence, you write smart contract code in Solidity, and the compiler converts it into bytecode. This bytecode is then deployed and stored on Ethereum (and other EVM-compatible blockchains).

For a basic introduction to the EVM and bytecode, you can check out this video I made.

### Understadning Blockchain Basics

For programmers, understanding blockchains is relatively straightforward. The complexities like mining, hashing, and cryptography are designed to provide specific features and promises. Similar to how you don’t need to know Amazon’s AWS internals to use it, you don't need to delve into blockchain’s technical details to use it.

**Transactions**
A blockchain is a globally shared, transactional database. Anyone can read entries by joining the network, but to make changes, a transaction must be created and accepted by the network. Transactions ensure changes are either fully applied or not at all, maintaining data consistency and integrity. Each transaction is cryptographically signed by the sender to secure access and modifications.

**Blocks**

Blocks address issues like double-spend attacks by establishing a globally accepted transaction order. Transactions are bundled into blocks, executed, and distributed among all nodes. If conflicting transactions exist, only the first accepted one is valid. Blocks form a linear sequence, giving rise to the term "blockchain." Blocks may occasionally be reverted, but the likelihood decreases as more blocks are added on top.

**Ethereum Virtual Machine**

The EVM is the runtime environment for Ethereum smart contracts. It is isolated, meaning code running inside the EVM has no access to the network, filesystem, or other processes. There are two types of accounts: external accounts (controlled by key pairs) and contract accounts (controlled by code). Both types share the same address space and have a balance in Ether.

**Transactions in EVM**

Transactions are messages from one account to another, possibly including binary data (payload) and Ether. If the target account contains code, it is executed with the payload as input. If creating a contract, the transaction's payload is the EVM bytecode, which is executed to produce the contract’s code.

**Gas**
Each transaction requires gas, paid by the originator, to execute. Gas incentivizes efficient use of EVM execution time and compensates executors (miners/stakers). If gas runs out during execution, an out-of-gas exception occurs, reverting state changes. The gas price is set by the transaction originator, limiting abuse and ensuring fair compensation.

**Storage, Memory, and the Stack**

The EVM uses three data areas:

- `Storage`: Persistent key-value store unique to each account.
- `Memory`: Freshly cleared for each message call, linear, and costly as it grows.
- `Stack`: 1024 elements, 256-bit words, used for computations with limited top-end access.
Understanding these basics will help you grasp how blockchain technology and Ethereum smart contracts function.

### What is a Smart Contract?

Here's a simple smart contract example to help you understand the basics of Solidity. It may seem basic, but it will provide you with a lot of foundational knowledge.

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.18;

contract Count {
    uint public count;

    // Get the current count
    function get() public view returns (uint) {
        return count;
    }

    // Increment count quantity by 1
    function increment() public {
        count += 1;
    }

    // Function to decrement count by 1
    function decrement() public {
        count -= 1; // What happens if count = 0 when this function is called?
    }
}
```

We'll delve into details like what `public` and `view` mean shortly. For now, here are seven key learnings from the example above:

1. The first comment line (`// SPDX-License-Identifier: MIT`) specifies the licensing that covers the code.
2. The `pragma` directive, which must be the first line of code, tells the compiler which version to use.
3. Solidity is frequently updated, so different versions of the compiler produce different results.
4. Semicolons are essential in Solidity; the compiler will fail if even one is missing.
5. The `contract` keyword tells the compiler you're declaring a smart contract.
6. Functions encapsulate single ideas, specific functionality, tasks, etc. They should do one thing at a time.
7. The `count` variable is a state variable, holding the contract’s state data that persists on the blockchain.

### Declaring Variables and Functions in Solidity

To understand more about the structure of Solidity, let’s break down the `Count` smart contract.

**State Variable Declaration:**

```solidity
uint public count;
```

This line declares a state variable called `count` that can only store unsigned integers (positive whole numbers).

**Function Declaration:**

```solidity
function get() public view returns (uint) {
    return count;
}
```

This function returns the value of the state variable `count`. You might already be familiar with this concept from Object Oriented Programming which are getters.

**Visibility Specifiers:**

- `public`: The function can be accessed both internally and externally.
- `view`: The function does not modify the state.

State variables can also be `constant` or `immutable`:

```solidity
string constant TEXT = "abc";
address immutable owner = 0xD4a33860578De61DBAbDc8BFdb98FD742fA7028e;
```

Constants and immutable variables can only be assigned once and cannot be modified thereafter.

### Variable Scope in Smart Contracts

Solidity has three scopes for variables:

1. **State Variables:** Store permanent data on the blockchain.
2. **Local Variables:** Temporary data used within functions.
3. **Global Variables:** Provided by Solidity, giving information about the blockchain environment and utility functions.

You can distinguish these scopes as follows:

- State variables are inside the smart contract but outside any function.
- Local variables are within functions and not accessible outside their scope.
- Global variables are automatically available.

### Visibility Specifiers

Visibility in Solidity refers to the accessibility of variables, functions, or contracts. There are four types:

1. **Public:** Accessible from anywhere.
2. **Private:** Accessible only within the declaring contract.
3. **Internal:** Accessible within the declaring contract and derived contracts.
4. **External:** Functions that can only be called from outside the declaring contract.

### What are Constructors?

A constructor is a special type of function in Solidity that is executed only once when a smart contract is created. It is used to initialize the contract’s state and set up any necessary initial parameters. Here’s an example:

```solidity
contract Example {
    string public name;

    constructor(string memory _name) {
        name = _name;
    }
}
```

In this example, the constructor function takes a single parameter `_name` and assigns it to the state variable `name`. This initialization happens only once, at the time the contract is deployed.

**Example Use Case:**

Consider a scenario where you want to deploy a contract that represents different types of tokens. Instead of creating separate contracts for each token type, you can use a constructor to set the token’s name and symbol during deployment:

```solidity
contract Token {
    string public name;
    string public symbol;

    constructor(string memory _name, string memory _symbol) {
        name = _name;
        symbol = _symbol;
    }
}
```

By passing the token’s name and symbol as parameters to the constructor, you can deploy multiple instances of the Token contract with different values, making the contract more versatile and efficient.

### Interfaces and Abstract Contracts

Interfaces define a set of function signatures without implementation, while abstract contracts can have some implemented functions but must have at least one unimplemented function.

**Interface Example:**

```solidity
interface IHotFudgeSauce {
    function get() external view returns (uint);
    function increment() external;
    function decrement() external;
}
```

**Abstract Contract Example:**

```solidity
abstract contract Feline {
    int public age;

    function utterance() public virtual returns (bytes32);

    function setAge(int _age) public {
        age = _age;
    }
}
```

### Summary

Solidity is a powerful language for writing smart contracts on the Ethereum blockchain. Understanding its syntax, visibility specifiers, and the structure of smart contracts is crucial for developing efficient and secure blockchain applications.


