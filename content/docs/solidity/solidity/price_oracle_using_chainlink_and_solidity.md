---
title: "Price Oracle Using Chainlink and Solidity"
description: "Get access to price feed ofchain using solidity and chainlink."
icon: "api"
draft: false
---

One application of Solidity is in decentralized finance (DeFi) and obtaining accurate and reliable price data is crucial for various operations such as asset valuation, trading, and lending. Chainlink, a decentralized oracle network, provides a solution to this problem by offering decentralized price oracles. In this section, we'll explore a Solidity contract that interacts with a Chainlink price oracle to fetch the latest ETH/USD price.

Chainlink price oracles are decentralized services that provide tamper-resistant price data to smart contracts on the blockchain. These oracles fetch data from multiple trusted sources, aggregate it, and make it available on-chain for various use cases.

### Contract Overview

Let's dissect the Solidity contract `ChainlinkPriceOracle`:

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

contract ChainlinkPriceOracle {
    AggregatorV3Interface internal priceFeed;

    constructor() {
        // ETH / USD
        priceFeed =
            AggregatorV3Interface(0x5f4eC3Df9cbd43714FE2740f5E3616155c5b8419);
    }

    function getLatestPrice() public view returns (int256) {
        (
            uint80 roundID,
            int256 price,
            uint256 startedAt,
            uint256 timeStamp,
            uint80 answeredInRound
        ) = priceFeed.latestRoundData();
        // for ETH / USD price is scaled up by 10 ** 8
        return price / 1e8;
    }
}

interface AggregatorV3Interface {
    function latestRoundData()
        external
        view
        returns (
            uint80 roundId,
            int256 answer,
            uint256 startedAt,
            uint256 updatedAt,
            uint80 answeredInRound
        );
}
```

### Explaining the Contract

1. **Contract Initialization:**
   ```solidity
   constructor() {
       // ETH / USD
       priceFeed =
           AggregatorV3Interface(0x5f4eC3Df9cbd43714FE2740f5E3616155c5b8419);
   }
   ```
   - In the constructor, the contract initializes the `priceFeed` variable with the address of the Chainlink aggregator contract for the ETH/USD price feed.

2. **Fetching Latest Price:**
   ```solidity
   function getLatestPrice() public view returns (int256) {
       (
           uint80 roundID,
           int256 price,
           uint256 startedAt,
           uint256 timeStamp,
           uint80 answeredInRound
       ) = priceFeed.latestRoundData();
       // for ETH / USD price is scaled up by 10 ** 8
       return price / 1e8;
   }
   ```
   - The `getLatestPrice` function fetches the latest ETH/USD price from the Chainlink aggregator contract.
   - It calls the `latestRoundData` function of the `priceFeed` interface, which returns various information including the latest price.
   - Since the price is scaled up by 10^8 in the Chainlink aggregator contract, we divide it by `1e8` to get the actual price.

3. **Aggregator Interface:**
   ```solidity
   interface AggregatorV3Interface {
       function latestRoundData()
           external
           view
           returns (
               uint80 roundId,
               int256 answer,
               uint256 startedAt,
               uint256 updatedAt,
               uint80 answeredInRound
           );
   }
   ```
   - Defines the interface for interacting with Chainlink price aggregator contracts.
   - It specifies the `latestRoundData` function, which returns various details including the latest price.

### Conclusion

The `ChainlinkPriceOracle` contract demonstrates how to integrate Chainlink's decentralized price oracles into Solidity smart contracts. By utilizing Chainlink's trusted price data, developers can build DeFi applications that rely on accurate and reliable price feeds for their operations. This contract serves as a foundational piece in the ecosystem of decentralized finance, enabling secure and transparent financial transactions on the blockchain.