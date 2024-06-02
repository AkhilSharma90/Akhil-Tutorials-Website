---
title: "Build a Remote Purchase Contract Using Solidity"
description: "Build a contract that will allow users to vote for anyone they choose."
icon: "api"
draft: false
---

Purchasing goods remotely currently requires multiple parties that need to trust each other. The simplest configuration involves a seller and a buyer. The buyer would like to receive an item from the seller, and the seller would like to get some compensation, e.g., Ether, in return. The problematic part is the shipment: There is no way to determine for sure that the item arrived at the buyer.

There are multiple ways to solve this problem, but all fall short in one way or another. In the following example, both parties have to put twice the value of the item into the contract as escrow. Once this happens, the Ether will stay locked inside the contract until the buyer confirms they received the item. After that, the buyer is returned the value (half of their deposit), and the seller gets three times the value (their deposit plus the value). The idea behind this is that both parties have an incentive to resolve the situation; otherwise, their Ether is locked forever.

This contract does not solve the problem entirely but gives an overview of how you can use state machine-like constructs inside a contract.

## Smart Contract Code Breakdown

Here is the complete code for the smart contract and a breakdown of its key parts.

### Contract Initialization

```solidity
// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.4;

contract Purchase {
    uint public value;
    address payable public seller;
    address payable public buyer;

    enum State { Created, Locked, Release, Inactive }
    State public state;

    modifier condition(bool condition_) {
        require(condition_);
        _;
    }

    error OnlyBuyer();
    error OnlySeller();
    error InvalidState();
    error ValueNotEven();

    modifier onlyBuyer() {
        if (msg.sender != buyer)
            revert OnlyBuyer();
        _;
    }

    modifier onlySeller() {
        if (msg.sender != seller)
            revert OnlySeller();
        _;
    }

    modifier inState(State state_) {
        if (state != state_)
            revert InvalidState();
        _;
    }

    event Aborted();
    event PurchaseConfirmed();
    event ItemReceived();
    event SellerRefunded();
```

- **State Variables**: `value`, `seller`, and `buyer` are defined to store the item's value and the addresses of the seller and buyer.
- **Enum `State`**: Represents the different states of the contract: `Created`, `Locked`, `Release`, and `Inactive`.
- **Modifiers and Errors**: Defined to restrict access to certain functions and to handle errors gracefully.

### Constructor

```solidity
    constructor() payable {
        seller = payable(msg.sender);
        value = msg.value / 2;
        if ((2 * value) != msg.value)
            revert ValueNotEven();
    }
```

- **Constructor**: Sets the seller's address and ensures the value sent is even, as it will be divided into two equal parts for the escrow.

### Aborting the Purchase

```solidity
    function abort()
        external
        onlySeller
        inState(State.Created)
    {
        emit Aborted();
        state = State.Inactive;
        seller.transfer(address(this).balance);
    }
```

- **abort**: Allows the seller to abort the transaction before it is locked, refunding the seller and setting the contract to an inactive state.

### Confirming the Purchase

```solidity
    function confirmPurchase()
        external
        inState(State.Created)
        condition(msg.value == (2 * value))
        payable
    {
        emit PurchaseConfirmed();
        buyer = payable(msg.sender);
        state = State.Locked;
    }
```

- **confirmPurchase**: Allows the buyer to confirm the purchase, requiring a payment of twice the item's value. This locks the Ether in the contract.

### Confirming Receipt of Item

```solidity
    function confirmReceived()
        external
        onlyBuyer
        inState(State.Locked)
    {
        emit ItemReceived();
        state = State.Release;
        buyer.transfer(value);
    }
```

- **confirmReceived**: Allows the buyer to confirm receipt of the item, releasing their escrowed funds and moving to the release state.

### Refunding the Seller

```solidity
    function refundSeller()
        external
        onlySeller
        inState(State.Release)
    {
        emit SellerRefunded();
        state = State.Inactive;
        seller.transfer(3 * value);
    }
}
```

- **refundSeller**: Allows the seller to retrieve their escrowed funds once the buyer confirms receipt, moving the contract to an inactive state.

## Conclusion

This smart contract example demonstrates a way to create a trustless escrow system using Ethereum. Both parties must put up collateral, incentivizing them to complete the transaction honestly. Although this solution doesn't solve all potential issues with remote purchases, it provides a foundation for using state machines within smart contracts.
