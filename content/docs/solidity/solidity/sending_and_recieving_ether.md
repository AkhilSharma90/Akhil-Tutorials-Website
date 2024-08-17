---
title: "How to Send and Receive Ether in Solidity"
description: "A simple guide on how to send and receive ether in solidity."
icon: "api"
draft: false
---

Sending and receiving Ether in Solidity involves understanding the different methods available and their respective use cases. This guide will cover how to send Ether using `transfer`, `send`, and `call`, how to receive Ether, and which method to use for optimal security.

---

### Sending Ether

##### transfer

The `transfer` method sends 2300 gas and throws an error if the transfer fails.

```solidity
function sendViaTransfer(address payable _to) public payable {
    // This function is no longer recommended for sending Ether.
    _to.transfer(msg.value);
}
```

##### send

The `send` method also sends 2300 gas but returns a boolean indicating success or failure.

```solidity
function sendViaSend(address payable _to) public payable {
    // Send returns a boolean value indicating success or failure.
    // This function is not recommended for sending Ether.
    bool sent = _to.send(msg.value);
    require(sent, "Failed to send Ether");
}
```

##### call

The `call` method forwards all gas or sets a specified amount and returns a boolean indicating success or failure. This is the recommended method.

```solidity
function sendViaCall(address payable _to) public payable {
    // Call returns a boolean value indicating success or failure.
    // This is the current recommended method to use.
    (bool sent, bytes memory data) = _to.call{value: msg.value}("");
    require(sent, "Failed to send Ether");
}
```

### Receiving Ether

A contract that receives Ether must implement at least one of the following functions:

- `receive() external payable`
- `fallback() external payable`

The `receive()` function is called if `msg.data` is empty, otherwise, the `fallback()` function is called.

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

contract ReceiveEther {
    // Function to receive Ether. msg.data must be empty
    receive() external payable {}

    // Fallback function is called when msg.data is not empty
    fallback() external payable {}

    function getBalance() public view returns (uint256) {
        return address(this).balance;
    }
}
```

### Which Method Should You Use?

As of December 2019, using `call` in combination with a re-entrancy guard is the recommended method for sending Ether.

To guard against re-entrancy attacks:

1. Make all state changes before calling other contracts.
2. Use a re-entrancy guard modifier.

We have a topic about reentrancy attacks and you can read more about it.

Example with re-entrancy guard:

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import {ReentrancyGuard} from "@openzeppelin/contracts/security/ReentrancyGuard.sol";

contract SendEther is ReentrancyGuard {
    function sendViaCall(address payable _to) public payable nonReentrant {
        // Call returns a boolean value indicating success or failure.
        (bool sent, bytes memory data) = _to.call{value: msg.value}("");
        require(sent, "Failed to send Ether");
    }
}
```

### Example Contracts

**ReceiveEther Contract**

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

contract ReceiveEther {
    // Function to receive Ether. msg.data must be empty
    receive() external payable {}

    // Fallback function is called when msg.data is not empty
    fallback() external payable {}

    function getBalance() public view returns (uint256) {
        return address(this).balance;
    }
}
```

**SendEther Contract**

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

contract SendEther {
    function sendViaTransfer(address payable _to) public payable {
        // This function is no longer recommended for sending Ether.
        _to.transfer(msg.value);
    }

    function sendViaSend(address payable _to) public payable {
        // Send returns a boolean value indicating success or failure.
        // This function is not recommended for sending Ether.
        bool sent = _to.send(msg.value);
        require(sent, "Failed to send Ether");
    }

    function sendViaCall(address payable _to) public payable {
        // Call returns a boolean value indicating success or failure.
        // This is the current recommended method to use.
        (bool sent, bytes memory data) = _to.call{value: msg.value}("");
        require(sent, "Failed to send Ether");
    }
}
```

By following these guidelines and using the recommended methods, you can securely send and receive Ether in your Solidity smart contracts, ensuring robust and secure transactions.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
