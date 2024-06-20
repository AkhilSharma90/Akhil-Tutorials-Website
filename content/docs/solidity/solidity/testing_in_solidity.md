---
title: "Learn How To Write Unit Tests In Solidity"
description: "Learn about Solidity, basics, language syntax and more."
icon: "api"
draft: false
---

Unit testing is a critical part of the development process, ensuring that your smart contracts behave as expected. In this section, we will explore how to write unit tests in Solidity using the Forge testing framework. We will start by understanding the concept of AAA (Arrange, Act, Assert) and then dive into a practical example.

## Understanding AAA (Arrange, Act, Assert)

The AAA pattern is a standard approach in unit testing that ensures tests are well-structured and easy to understand. It consists of three main steps:

1. **Arrange**: Set up the necessary preconditions and inputs.
2. **Act**: Perform the action that you want to test.
3. **Assert**: Verify that the action had the expected outcome.

This pattern helps in creating clear and maintainable tests. Let's see how this applies to Solidity testing with Forge.

## Example: Testing a Counter Contract

We will test a simple `Counter` contract (we have already covered this contracy in the introduction) that allows setting a number and incrementing it. Here is the `Counter` contract:

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

### Writing Unit Tests for the Counter Contract

Now, let's write unit tests for the `Counter` contract using the AAA pattern.

```solidity
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import {Test, console} from "forge-std/Test.sol";
import {Counter} from "../src/Counter.sol";

contract CounterTest is Test {
    Counter public counter;

    // Setting up the initial state of the contract once deployed
    function setUp() public {
        counter = new Counter();
        counter.setNumber(0);
    }

    // Act and Assert: Increment the counter and check the result
    function test_Increment() public {
        // Act
        counter.increment();

        // Assert
        assertEq(counter.number(), 1);
    }

    // Act and Assert: Set the number to a specific value and verify
    function testFuzz_SetNumber(uint256 x) public {
        // Act
        counter.setNumber(x);

        // Assert
        assertEq(counter.number(), x);
    }
}
```

### Explanation of the Test Code

Since this is not a complete contract, and we are using it to get to understand how to write unit tests, some functions will be simple and will not need all three AAA concepts.

We will started on with the imports:
```solidity
import {Test, console} from "forge-std/Test.sol";
import {Counter} from "../src/Counter.sol";

contract CounterTest is Test {
```
We are importing two contraccts: the Counter contract which we will be testing and the `Test` contract which will help us test these contract and provide us with utility functions.

The `setUp` function is used to initialize the `Counter` contract and set the number to `0`. This function runs before each test, ensuring a clean state.

1. **`test_Increment**:
   - **Act**: The `counter.increment()` function is called to increment the number.
   - **Assert**: The `assertEq(counter.number(), 1)` statement checks if the number is incremented to 1. If true, the tests will pass, else they will fail.

2. **(testFuzz_SetNumber)**:
   - **Act**: The `counter.setNumber(x)` function sets the number to a random value `x`.
   - **Assert**: The `assertEq(counter.number(), x)` statement verifies that the number is set correctly to `x`.

### Running the Tests

To run the tests, use the Forge framework. Ensure that you have Forge installed and set up. You can run the tests using the following command:

```bash
forge test
```

Forge will execute the tests and provide feedback on whether they pass or fail.

## Conclusion

Unit testing in Solidity is crucial for developing reliable smart contracts. Using the AAA pattern helps in structuring tests clearly and effectively. In this blog, we demonstrated how to write unit tests for a simple `Counter` contract using Forge. By following the AAA pattern, you can create well-structured and maintainable tests for your smart contracts.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).