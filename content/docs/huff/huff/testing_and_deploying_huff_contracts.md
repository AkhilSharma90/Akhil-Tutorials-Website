---
title: "Testing and Deploying Huff Contracts"
description: "Huff is a domain-specific, low-level programming language designed explicitly for writing smart contracts on the Ethereum blockchain."
icon: "code"
draft: false
---

In this tutorial, we will delve into the steps involved in testing and deploying a smart contract written using the Huff programming language.

## Introduction to Huff

For those unfamiliar with Huff, it is a low-level programming language designed for developing highly optimized smart contracts that run on the Ethereum Virtual Machine (EVM). Huff exposes the inner workings of the EVM, allowing developers to manually manipulate its programming stack.

The Aztec Protocol team originally created Huff to write Weierstrudel, an on-chain elliptical curve arithmetic library requiring incredibly optimized code that neither Solidity nor Yul could provide.

## The Math Contract

We will use a simple Huff contract that performs basic math operations like addition, subtraction, multiplication, etc.

```huff
#define macro ADD_NUMBERS() = takes (2) returns (1) {
    // Input stack:      // [num2, num1]
    add                  // [num2 + num1]         
}

#define macro SUB_NUMBERS() = takes (2) returns (1) {
    // Input stack:      // [num2, num1]
    swap1                // [num1, num2]
    sub                  // [num1 - num2]         
}

#define macro MULTIPLY_NUMBERS() = takes (2) returns (1) {
    // Input stack:      // [num2, num1]
    mul                  // [num2 * num1]         
}

#define macro DIVIDE_NUMBERS() = takes (2) returns (1) {
    // Input stack:      // [num2, num1]
    swap1                // [num1, num2]
    div                  // [num1 / num2]         
}

#define macro ABS() = takes (2) returns (1) {
    // Input stack:     // [num2, num1]
    dup1
    dup3
    lt 
    iszero swapAndSubtract jumpi
    sub                      
    complete jump  

    swapAndSubtract:
        swap1
        sub
    
    complete:
}
```

The above implementation is very basic, and there is plenty of room for improvements, but that’s a topic for another day.

## Testing the Contract

We can test the above contract in two ways: using Foundry and using Huff tests.

### Using Foundry

To test the above logic using Foundry, we need to deploy the wrapper contract, as it exposes the functions. We also need to use the HuffDeployer helper contract from the foundry-huff library.

Here’s how the tests should be set up:

1. Define the interface of the contract (`IMath`).
2. Deploy the wrapper contract in the `setUp()` method using the HuffDeployer.
3. Cast the returned address to the `IMath` interface.
4. Write tests as usual.

```solidity
/// Import HuffDeployer
import {HuffDeployer} from "foundry-huff/HuffDeployer.sol";

contract MathTest is Test {
    IMath public math;

    function setUp() public {
        address addr = HuffDeployer.deploy(
            "../test/foundry/wrappers/MathWrapper"
        );
        math = IMath(addr);
    }

    function testAddNumbers() public {
        uint256 result = math.addNumbers(420, 69);
        assertEq(result, 489);
    }

    function testAddNumbers_fuzz(uint256 a, uint256 b) public {
        unchecked {
            uint256 c = a + b;

            if (c > MAX) {
                vm.expectRevert();
                math.addNumbers(a, b);
                return;
            }

            uint256 result = math.addNumbers(a, b);
            assertEq(result, a + b);
        }
    }

    function testSubNumbers() public {
        uint256 result = math.subNumbers(420, 69);
        assertEq(result, 351);
    }

    // Add more tests as needed...
}
```

### Huff Tests

Testing Huff contracts using Foundry is the most commonly used method. However, we can also write simpler (and faster) unit tests using Huff itself. The Huff compiler (`huffc`) has a `test` command, which takes the filename containing the tests as an argument. We can use the `TestHelpers` utility created by Maddiaa for basic operations like `ASSERT`, etc.

A sample Huff test contract looks like this:

```solidity
// ./test/huff/Math.t.huff

/* Imports */
#include "./helpers/TestHelpers.huff"
#include "../../src/Math.huff"

/* Tests */
#define test TEST_ADD() = {
    0x01                // [1]
    0x02                // [2,1]
    ADD_NUMBERS()       // [sum]
    0x03                // [3,sum]
    ASSERT_EQ()         // [3 == sum]
    
    0x4563918244f40000  // [5e18]            
    0x4563918244f40000  // [5e18, 5e18]            
    ADD_NUMBERS()       // [SUM]    
    0x8ac7230489e80000  // [10e18, SUM]             
    ASSERT_EQ()         // [10e18 == SUM]     
}
```
We can run the tests using the command:

```bash
$ huffc ./test/Math.t.huff test
```

## Deploying Huff Contracts

We have seen how to test Huff contracts using Foundry and Huff. Now, we will move on to the next step: deploying the Huff contract to an EVM-based blockchain (Goerli, in this case). Here’s the Foundry script to deploy the `MathWrapper` contract:

```solidity
// scripts/DeployMath.s.sol

// SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.15;

import "foundry-huff/HuffDeployer.sol";
import "forge-std/Script.sol";
import {IMath} from "../src/interfaces/IMath.sol";

contract Deploy is Script {
  function run() public returns (IMath math) {
    math = IMath(HuffDeployer.broadcast("wrappers/MathWrapper"));
    console2.log("MathWrapper contract deployed to: ", address(math));
  }
}
```

The above script can be executed by running the following command:

```bash
$ source .env && forge script scripts/DeployMath.s.sol:DeployMath --fork-url $RPC_URL --private-key $PRIVATE_KEY --broadcast
```

You need to configure the `RPC_URL` of the network and `PRIVATE_KEY` of the deployer in the `.env` file.

To validate the deployment, you can either add assertions in the script’s `run()` method or implement fork tests with the deployed contract address.

We have successfully tested and deployed our simple `Math.huff` contract. You can follow the same process to test and deploy more complex Huff contracts as well.