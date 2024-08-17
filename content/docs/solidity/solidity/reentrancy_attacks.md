---
title: "Learn About Reentracny Attacks and how to Prevent them"
description: "Learn how blockchain reentrancy attacks work and how to protect your smart contracts from them."
icon: "api"
draft: false
---

### What is a Solidity Reentrancy Attack?

In Solidity smart contracts, a reentrancy attack occurs when an external contract is called, allowing the function to be recursively called before its initial execution is complete. This can enable the external contract to manipulate the state of the original contract before it finishes executing.

Reentrancy attacks exploit state synchronization issues, occurring when the state is not updated before making an external call. For instance, if a function checks a condition, updates the state, and then makes an external call, an attacker can re-enter the function in the middle of its execution, bypassing the updated state and potentially draining funds.

### Types of Smart Contract Reentrancy Attacks

**1. Single Function Reentrancy**

This basic form of reentrancy attack involves a single function within a contract being re-entered. This typically happens when the function modifies the contract’s state and then calls an external contract or sends Ether without first updating its internal state variables.

**2. Cross-Function Reentrancy**

In cross-function reentrancy, one function performs an external call before updating the state, and the external contract calls another function that depends on this state. This can lead to unexpected interactions, allowing an attacker to manipulate the contract’s state across multiple functions.

**3. Cross-Contract Reentrancy**

Cross-contract reentrancy involves interactions between functions in multiple contracts where the state is shared. If the shared state in the first contract is not updated before an external call, other contracts that depend on the shared state can be re-entered.

**5. Cross-Chain Reentrancy**

Cross-chain reentrancy, though less common, involves interactions between smart contracts on different blockchain networks. This scenario arises in interoperability protocols or decentralized exchanges (DEXs) facilitating transactions across multiple blockchains.

**6. Read-Only Reentrancy**

Read-only reentrancy occurs when an external call is made to another contract, but the called contract’s function does not modify its state. Instead, the called function reads data from the calling contract and then reenters it, potentially causing unexpected behavior.

### How to prevent Solidity Reentrancy Attacks

##### Use the Checks-Effects-Interactions Pattern

Ensure that state changes are made before interacting with external contracts or sending Ether. Modify the execution order to follow the Checks-Effects-Interactions pattern:

1. **Checks:** Verify the state of the caller.
2. **Effects:** Update the global state.
3. **Interactions:** Perform the external call.

**Example:**

```solidity
mapping(address => uint) public balance;

function withdraw(uint amount) public {
    // 1. Checks
    require(balance[msg.sender] >= amount);
    // 2. Effects
    balance[msg.sender] -= amount;
    // 3. Interactions
    msg.sender.call{value: amount}("");
    emit Withdrawal(msg.sender, amount);
}
```

###### Implement Mutexes or Locks

A mutex (mutual exclusion) mechanism prevents a function from being executed multiple times within the same transaction. This is typically done using a boolean flag.

**Example Using Reentrancy Guard:**

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.18;
import {ReentrancyGuard} from "@openzeppelin/contracts/security/ReentrancyGuard.sol";

contract ReentracyProtected is ReentrancyGuard {
    mapping(address => uint) public balances;

    function withdraw() external nonReentrant {
        uint balance = balances[msg.sender];
        require(balance > 0, "Insufficient balance");
        balances[msg.sender] = 0;
        (bool success, ) = address(msg.sender).call{ value: balance }("");
        require(success, "Failed to withdraw");
    }
}
```

#### Perform Extensive Code Review and Testing

- **Audits:** Multiple rounds of smart contract audits can significantly decrease the chance of a reentrancy attack.
- **Thorough Testing:** Beyond audits, thorough testing, including invariant testing, is crucial to ensure smart contract security.

---

### Examples of Smart Contract Reentrancy Attacks

**The DAO Hack:** In 2016, the DAO, a decentralized investment fund, was exploited due to a reentrancy vulnerability. The attacker was able to repeatedly withdraw funds before the contract could update its balance, resulting in the theft of approximately $6 million worth of Ether.

**Curve Finance:** On July 30th, 2023, Curve Finance, a decentralized finance (DeFi) protocol, was attacked due to a Vyper compiler bug, leading to a loss of nearly $70 million.

### Reentrancy Example

A vulnerability is found in the `HypercertMinter::splitValue` function, which calls `_mintBatch()` before updating the storage. This violates the Checks-Effects-Interactions pattern, making it vulnerable to a reentrancy attack.

**Vulnerable Code:**

```solidity
function _splitValue(address _account, uint256 _tokenID, uint256[] calldata _values) internal {
    uint256 valueLeft = tokenValues[_tokenID];
    for (uint256 i; i < len;) {
        valueLeft -= values[i];
        tokenValues[toIDs[i]] = values[i];
        unchecked { ++i; }
    }
    _mintBatch(_account, toIDs, amounts, "");
    tokenValues[_tokenID] = valueLeft;
    emit BatchValueTransfer(typeIDs, fromIDs, toIDs, values);
}
```

The above code is susceptible to reentrancy attacks as the state is not updated before the external call to `_mintBatch()`, allowing an attacker to exploit this vulnerability.

---

### Conclusion

Reentrancy attacks are a significant vulnerability in Solidity smart contracts, enabling attackers to manipulate contract states and potentially drain funds. This article explored the mechanics of reentrancy attacks, their types, mitigation strategies, and real-world examples like the DAO hack and Curve Finance incident. Emphasizing the importance of security measures and thorough auditing is crucial in preventing such attacks.

By understanding and implementing these strategies, developers can secure their smart contracts against reentrancy attacks and ensure the integrity of their blockchain applications.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
