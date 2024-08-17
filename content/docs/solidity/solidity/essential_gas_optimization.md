---
title: "Essential Gas Optimization Techniques in Solidity"
description: "Gas optimization is key in smart contract and will prevent your protocols from becoming very expensive to make requests since gas prices would be very high."
icon: "api"
draft: false
---

Ethereum gas fees have long been a concern for users. Although the recent Ethereum Proof-of-Stake merge introduced a more energy-efficient system, it had little effect on gas fees. To maintain high standards, minimize risk, write clean code, and create secure, cost-effective smart contracts, it is critical to know the techniques for optimizing gas with Solidity.

In this section on the best Solidity gas optimization tips and techniques, you will learn advanced, real-world, and tested strategies taught by top-notch web3 developers to reduce the gas costs of your smart contracts.

### Understanding Gas and Gas Optimization in Solidity

Gas is the unit of measurement for the computational effort required to perform operations on the Ethereum network. Solidity gas optimization involves making your smart contract code less expensive to execute.

Every Ethereum transaction requires computational resources, and the fee for these resources is referred to as gas. When a smart contract is compiled, it is converted into a series of "operation codes" or opcodes. Each opcode has a predefined gas cost, representing the computational work needed for that operation.

The goal of optimization is to reduce the number of operations needed to run a smart contract. Optimized contracts not only lower gas costs but also protect against malicious misuse.

### Key Solidity Gas Optimization Techniques

#### 1. Use Mappings Instead of Arrays

Arrays and mappings are two data types in Solidity used to describe lists of data. While arrays are packable and iterable, mappings are less expensive.

**Array Example:**

```solidity
string languages[];
languages = ["go", "python", "solidity"];
```

**Mapping Example:**

```solidity
mapping(uint => string) public languages;

constructor() public {
    languages[0] = "go";
    languages[1] = "python";
    languages[2] = "solidity";
}
```

Use mappings to manage data lists to conserve gas, except when iteration is required or data types can be packed. Mappings allow direct access to values without iteration.

#### 2. Enable the Solidity Compiler Optimizer

The Solidity compiler optimizer simplifies complex expressions, reducing code size and execution costs. It optimizes inline operations, deployment costs, and function call costs.

**Example Settings:**

```solidity
module.exports = {
  solidity: {
    version: "0.8.9",
    settings: {
      optimizer: {
        enabled: true,
        runs: 10000,
      },
    },
  },
};
```

#### 3. Minimize On-Chain Data

Reducing on-chain data storage lowers gas costs. Save only critical data on-chain and keep other data off-chain. Avoid looping through large arrays and batch operations to reduce gas consumption.

#### 4. Use Indexed Events

Events in Solidity notify users of blockchain activities. Indexed events can be searched using indexed parameters as filters, improving efficiency and reducing overall gas usage.

**Example:**

```solidity
event myFirstEvent(address indexed sender, uint256 indexed amount, string message);
```

#### 5. Be Cautious with uint8

Using uint8 can increase gas costs because the EVM handles 32 bytes at a time. For storage values, using uint256 is often more efficient unless multiple small variables can be packed into a single storage slot.

**Less Efficient:**

```solidity
contract A { uint8 a = 0; }
```

**More Efficient:**

```solidity
contract A { uint a = 0; // or uint256 }
```

#### 6. Pack Your Variables

Pack small-sized state variables sequentially to save storage space. This reduces gas costs by combining multiple reads or writes into a single operation.

**Before:**

```solidity
contract MyContract {
  uint128 c;
  uint256 b;
  uint128 a;
}
```

**After:**

```solidity
contract Leggo {
  uint128 a;
  uint128 c;
  uint256 b;
}
```

#### 7. Free Up Unused Storage

Deleting unused variables frees up space and earns a gas refund. Use the `delete` keyword or assign default values to remove unused storage.

**Example:**

```solidity
delete myVariable; // or myInt = 0;
```

#### 8. Store Data in Calldata Instead of Memory

For certain function parameters, storing data in `calldata` instead of `memory` is more cost-effective if the data only needs to be read.

**Calldata Example:**

```solidity
function func2(uint[] calldata nums) external {
  for (uint i = 0; i < nums.length; ++i) {
    ...
  }
}
```

**Memory Example:**

```solidity
function func1(uint[] memory nums) external {
  for (uint i = 0; i < nums.length; ++i) {
    ...
  }
}
```

#### 9. Use Immutable and Constant

Use `immutable` and `constant` keywords to limit changes to state variables. `Constant` variables cannot be changed after compilation, while `immutable` variables can be set within the constructor.

**Example:**

```solidity
contract MyContract {
    uint256 constant b = 10;
    uint256 immutable a;

    constructor() {
        a = 5;
    }
}
```

#### 10. Use the External Visibility Modifier

Using the `external` function visibility modifier can optimize gas usage. Unlike `public`, `external` functions are less costly to call from outside the contract.

**Example:**

```solidity
function one() public view returns (string memory) {
  return message;
}

function two() external view returns (string memory) {
  return message;
}
```

By mastering these techniques, you can create efficient, cost-effective smart contracts that optimize gas usage and enhance the overall performance of your Ethereum-based applications.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
