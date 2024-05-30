---
title: "Build A Crowdfunding Contract with Solidity and ERC20 Tokens"
description: "In this section, you will build a simple crowdfunding contract using solidity and get to implement some of Solidity concepts like interfaces, struct and more."
icon: "api"
draft: false
---

In decentralized finance (DeFi), crowdfunding has become increasingly popular as a means for raising funds for various projects. In this section, we'll explore how to create a crowdfunding contract on Ethereum using ERC20 tokens. The contract allows users to create campaigns, pledge tokens to campaigns, and claim funds if the campaign goal is reached. If the goal is not met, users can withdraw their pledged tokens.

### Contract Overview

Our crowdfunding contract consists of the following functionalities:

- Campaign Creation: Users can create campaigns specifying the fundraising goal and duration.
- Pledging: Contributors can pledge ERC20 tokens to campaigns.
- Claiming Funds: Campaign creators can claim the pledged funds if the goal is met.
- Refunding: Contributors can withdraw their pledged tokens if the campaign goal is not reached.

Let's start creating our contract.

Certainly! Let's break down the code into blocks and explain each part.

### 1: Importing Interfaces and Declaring Contract

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

interface IERC20 {
    function transfer(address, uint256) external returns (bool);
    function transferFrom(address, address, uint256) external returns (bool);
}

contract CrowdFund {
    // Contract code goes here...
}
```

- The contract starts with specifying the SPDX license identifier and the Solidity version pragma.
- It imports the `IERC20` interface, which defines the functions required for interacting with ERC20 tokens.
- The `CrowdFund` contract is declared.

### 2: Event Declarations

```solidity
event Launch(uint256 id, address indexed creator, uint256 goal, uint32 startAt, uint32 endAt);
event Cancel(uint256 id);
event Pledge(uint256 indexed id, address indexed caller, uint256 amount);
event Unpledge(uint256 indexed id, address indexed caller, uint256 amount);
event Claim(uint256 id);
event Refund(uint256 id, address indexed caller, uint256 amount);
```

- Events are declared to emit information about different actions in the contract.
- Events provide a way for external applications to listen and react to specific actions that occur within the contract.

### 3: Campaign Struct Declaration

```solidity
struct Campaign {
    address creator;
    uint256 goal;
    uint256 pledged;
    uint32 startAt;
    uint32 endAt;
    bool claimed;
}
```

- Defines a struct `Campaign` to represent crowdfunding campaigns.
- It contains fields for the campaign creator, fundraising goal, total amount pledged, start and end timestamps, and a flag indicating whether the funds have been claimed.

### 4: Contract State Variables

```solidity
IERC20 public immutable token;
uint256 public count;
mapping(uint256 => Campaign) public campaigns;
mapping(uint256 => mapping(address => uint256)) public pledgedAmount;
```

- Declares state variables:
  - `token`: Holds the address of the ERC20 token used for pledges.
  - `count`: Keeps track of the total number of campaigns created.
  - `campaigns`: Maps campaign IDs to their respective `Campaign` struct instances.
  - `pledgedAmount`: Maps campaign IDs to pledger addresses and the amount they have pledged.

### 5: Constructor

```solidity
constructor(address _token) {
    token = IERC20(_token);
}
```

- Constructor function initializes the `token` variable with the address of the ERC20 token contract.

### 6: Campaign Creation Function (`launch`)

```solidity
function launch(uint256 _goal, uint32 _startAt, uint32 _endAt) external {
        require(_startAt >= block.timestamp, "start at < now");
        require(_endAt >= _startAt, "end at < start at");
        require(_endAt <= block.timestamp + 90 days, "end at > max duration");

        count += 1;
        campaigns[count] = Campaign({
            creator: msg.sender,
            goal: _goal,
            pledged: 0,
            startAt: _startAt,
            endAt: _endAt,
            claimed: false
        });

        emit Launch(count, msg.sender, _goal, _startAt, _endAt);
    }
```

- Allows users to create a new crowdfunding campaign with a specified goal and duration. You can see proper error handling, campaign initialization and events.

### 7: Campaign Cancellation Function (`cancel`)

```solidity
function cancel(uint256 _id) external {
        Campaign memory campaign = campaigns[_id];
        require(campaign.creator == msg.sender, "not creator");
        require(block.timestamp < campaign.startAt, "started");

        delete campaigns[_id];
        emit Cancel(_id);
    }
```

- Allows the creator of a campaign to cancel it before it starts.

### 8: Pledge Function (`pledge`)

```solidity
function pledge(uint256 _id, uint256 _amount) external {
    Campaign storage campaign = campaigns[_id];
        require(block.timestamp >= campaign.startAt, "not started");
        require(block.timestamp <= campaign.endAt, "ended");

        campaign.pledged += _amount;
        pledgedAmount[_id][msg.sender] += _amount;
        token.transferFrom(msg.sender, address(this), _amount);

        emit Pledge(_id, msg.sender, _amount);
}
```

- Enables users to pledge tokens to a specific campaign.

### 9: Unpledge Function (`unpledge`)

```solidity
function unpledge(uint256 _id, uint256 _amount) external {
        Campaign storage campaign = campaigns[_id];
        require(block.timestamp <= campaign.endAt, "ended");

        campaign.pledged -= _amount;
        pledgedAmount[_id][msg.sender] -= _amount;
        token.transfer(msg.sender, _amount);

        emit Unpledge(_id, msg.sender, _amount);
    }
```

- Allows users to withdraw their pledged tokens from a campaign.

### 10: Claim Function (`claim`)

```solidity
function claim(uint256 _id) external {
        Campaign storage campaign = campaigns[_id];
        require(campaign.creator == msg.sender, "not creator");
        require(block.timestamp > campaign.endAt, "not ended");
        require(campaign.pledged >= campaign.goal, "pledged < goal");
        require(!campaign.claimed, "claimed");

        campaign.claimed = true;
        token.transfer(campaign.creator, campaign.pledged);

        emit Claim(_id);
    }
```

- Enables the creator of a campaign to claim the pledged funds if the goal is met.

### 11: Refund Function (`refund`)

```solidity
function refund(uint256 _id) external {
        Campaign memory campaign = campaigns[_id];
        require(block.timestamp > campaign.endAt, "not ended");
        require(campaign.pledged < campaign.goal, "pledged >= goal");

        uint256 bal = pledgedAmount[_id][msg.sender];
        pledgedAmount[_id][msg.sender] = 0;
        token.transfer(msg.sender, bal);

        emit Refund(_id, msg.sender, bal);
    }
```

- Allows contributors to refund their pledged tokens if the campaign goal is not met.

### Conclusion

Each block of the contract code represents a specific functionality of the crowdfunding contract, from campaign creation to fund claiming and refunding. Understanding each block's purpose and logic is essential for building and interacting with the contract effectively.
