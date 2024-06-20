---
title: "Effective error handling in Solidity"
description: "Learn how to handle errors effectively in solidity using keywords like Require, Revert and Assert."
icon: "api"
draft: false
---

### Error Handling in Solidity - Require, Assert, Revert

Error handling in Solidity is essential for ensuring that smart contracts function correctly and securely. Solidity provides three main mechanisms for handling errors: `require()`, `assert()`, and `revert()`. Each of these has specific use cases and behaviors that make them suitable for different types of error handling scenarios.

#### Understanding Error Handling

When an error occurs in Solidity, the Ethereum Virtual Machine (EVM) reverts all changes made to the blockchain’s state during the current call and its sub-calls. This means that any modifications are undone, and the state is returned to its previous condition. However, there are exceptions with low-level functions like `delegatecall`, `send`, and `call`, where an error will return a boolean `false` instead of propagating the error but we won't be focusing on those.

#### Using `require()`

The `require()` function is used to validate inputs, return values, and conditions before proceeding with the main logic of the code. If the condition specified in `require()` is not met, the function throws an error and reverts the transaction.

```solidity
function requireExample() public pure {
    require(msg.value >= 1 ether, "you must pay me at least 1 ether!");
}
```

In this example, if the caller does not send at least 1 ether, the function reverts with the error message: “you must pay me at least 1 ether!”. The second argument to `require()` is optional but recommended for providing informative error messages. Note that while unused gas is returned, any gas used before the `require()` statement is not refunded, so it's best to use `require()` early in the function.

#### Using `assert()`

The `assert()` function is used to check for conditions that should never be false. If an `assert()` condition fails, it throws an error of type `Panic(uint256)` and reverts the transaction.

```solidity
contract ThrowMe {   
    function assertExample() public pure {
        assert(address(this).balance == 0);
        // Do something.
    }
}
```

The `assert()` function is typically used to check invariants—conditions that must always hold true. In the example, the contract is designed to ensure that its balance is always zero, which is validated using `assert()`. Unlike `require()`, `assert()` is often used internally to verify that the contract's state has not been corrupted.

#### Using `revert()`

The `revert()` function can be used for more complex conditional logic where the conditions for throwing an error are more elaborate. `revert()` can also be used to throw custom-defined errors, which can be more informative and cost-effective in terms of gas.

```solidity
contract ThrowMe {   
    // custom error
    error ThrowMe_BadInput(string errorMsg, uint inputNum);

    function revertExample(uint input) public pure {
        if (input < 1000 ) {
            revert ThrowMe_BadInput("Number must be an even number greater than 999", input);
        }

        if (input < 0) {
            revert("Negative numbers not allowed");
        }
    }
}
```

In the example, the first `revert()` call uses a custom error `ThrowMe_BadInput`, providing specific error details and input value. This makes the error more readable and traceable. The second `revert()` call uses a simple string message. In both cases, the transaction reverts, and any unused gas is returned to the caller.

#### Summary

- **`require()`**: Use for validating inputs, return values, and conditions. It reverts the transaction if the condition is not met and returns an optional error message.
- **`assert()`**: Use for checking invariants and conditions that should never be false. It reverts the transaction with a `Panic(uint256)` error if the condition fails.
- **`revert()`**: Use for complex error handling and throwing custom-defined errors. It reverts the transaction and can provide detailed error information.

By understanding and using these error-handling mechanisms appropriately, you can ensure that your Solidity smart contracts are robust, secure, and maintainable.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).