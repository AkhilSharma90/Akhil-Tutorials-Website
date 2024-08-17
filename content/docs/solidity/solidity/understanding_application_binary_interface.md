---
title: "Understandin Application Binary Interface (ABI) in Solidity"
description: "Learn about ABIs in solidity and their uses."
icon: "api"
draft: false
---

### What is a Smart Contract ABI

The ABI, or “Application Binary Interface,” of a smart contract defines the standard way to interact with contracts in the Ethereum ecosystem. It allows both humans and machines to interact with contracts on the blockchain, facilitating both user-to-contract and contract-to-contract interactions.

In this section, you'll learn what a smart contract ABI is and how to obtain it for interaction purposes.

### What does an ABI look like?

Consider the following example of a simple smart contract:

```solidity
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

contract Counter {
    uint256 public number;

    function setNumber(uint256 newNumber) public {
        number = newNumber;
    }

    function increment() public {
        number++;
    }
}
```

When compiled and deployed, the readable Solidity code is converted into a hexadecimal string, which is what gets stored on the blockchain. This is where the ABI comes in, providing a human-readable way to interact with this hexadecimal data.

The ABI for the above contract looks like this:

```json
[
  {
    "type": "function",
    "name": "increment",
    "inputs": [],
    "outputs": [],
    "stateMutability": "nonpayable"
  },
  {
    "type": "function",
    "name": "number",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256",
        "internalType": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "setNumber",
    "inputs": [
      {
        "name": "newNumber",
        "type": "uint256",
        "internalType": "uint256"
      }
    ],
    "outputs": [],
    "stateMutability": "nonpayable"
  }
]
```

This JSON structure provides all the information needed to interact with the contract's functions, such as their names, inputs, outputs, and state mutability.

**Is the ABI for humans or machines?**

The ABI bridges the gap between human-readable code and machine-readable bytecode. It allows humans to understand how to interact with smart contracts and enables machines to convert human-readable function calls into executable bytecode.

### Components of an ABI

An ABI includes several key pieces of information for each function within a smart contract:

- **Function Names:** Identify the functions within the contract.
- **Function Arguments:** Detail the type, order, and data structure of inputs.
- **Return Types:** Specify the data type returned by the function.
- **Events:** Describe the events in the contract and their parameters.

### How to get a smart contract ABI

You can generate a smart contract ABI using various tools such as Remix, Foundry, and Hardhat. Most development frameworks will automatically create the ABI when you compile the smart contract.

**Get the smart contract ABI using Remix**

After compiling a smart contract in Remix, you can find an ABI copy button that allows you to easily obtain the ABI.

**Get the smart contract ABI using Foundry**

In Foundry, after running `forge build` successfully, you can find the ABI in the compilation details located in the `out` folder.

**Get the smart contract ABI using Hardhat**

In Hardhat, you can find the ABI in the `artifacts/build-info` folder after compiling the contract with `npx hardhat compile`.

### Using a smart contract ABI on Solidity

In Solidity, you can use interfaces to provide the ABI implicitly. This helps the smart contract understand how to interact with another contract's functions.

```solidity
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

interface ICounter {
    function setNumber(uint256 newNumber) external;
    function increment() external;
}
```

With this interface, Solidity can generate the raw data needed to call the `setNumber` function of a `Counter` contract.

```solidity
ICounter(counter_address).setNumber(8);
```

Alternatively, you can call the function directly with raw data:

```solidity
counter_address.call(0x3fb5c1cb0000000000000000000000000000000000000000000000000000000000000008);
```

### Using the smart contract ABI in JavaScript

In JavaScript, you use the ABI to interact with smart contracts via libraries like `ethers.js`. Here’s an example setup:

```javascript
const { ethers } = require("ethers");
const provider = new ethers.providers.JsonRpcProvider();
const counterAddress = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045";

const counterABI = [
  {
    type: "function",
    name: "increment",
    inputs: [],
    outputs: [],
    stateMutability: "nonpayable",
  },
  {
    type: "function",
    name: "number",
    inputs: [],
    outputs: [
      {
        name: "",
        type: "uint256",
        internalType: "uint256",
      },
    ],
    stateMutability: "view",
  },
  {
    type: "function",
    name: "setNumber",
    inputs: [
      {
        name: "newNumber",
        type: "uint256",
        internalType: "uint256",
      },
    ],
    outputs: [],
    stateMutability: "nonpayable",
  },
];

const counterContract = new ethers.Contract(
  counterAddress,
  counterABI,
  provider
);

async function setNumber() {
  await counterContract.setNumber(8);
}
```

By using the ABI, `ethers.js` converts the human-readable function call `setNumber(8)` into the raw hexadecimal data required by the smart contract.

### Summary: What is a Smart Contract ABI

The ABI, or “Application Binary Interface,” is a crucial component that allows humans and machines to encode and decode data for interacting with smart contracts. It simplifies the process of translating human-readable code into machine-readable bytecode and vice versa, making it easier to develop and interact with complex Ethereum-based applications.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
