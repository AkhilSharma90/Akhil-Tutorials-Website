---
title: "Creating a Huff Contract for Signature Verification"
description: "Huff is a domain-specific, low-level programming language designed explicitly for writing smart contracts on the Ethereum blockchain."
icon: "code"
draft: false
---

## Overview

In this section, we aim to create a Huff smart contract that takes a signature as input from the calldata, verifies if the message was signed by the sender of the transaction, and returns true if it was. If the message wasn't signed by the sender or if the calldata doesn't adhere to the expected structure, the contract will cause the transaction to run out of gas.

## Solution

Here’s the optimized solution for quick reference:

```huff
#define macro MAIN() = takes (0) returns (0) {
    /// Check if calldatasize is 97 bytes (MessageHash=32, Signature=65)
    calldatasize 
    0x61 eq 
    extractParamsAndStore jumpi

    oog jump
        
    extractParamsAndStore:
    /// Store the message hash
    0x00 calldataload
    0x00 mstore

    /// Store 'v'
    0x60 calldataload
    0x3f mstore

    /// Store 'r'
    0x20 calldataload
    0x40 mstore 
    
    /// Store 's'
    0x40 calldataload
    0x60 mstore 
    
    /// Prepare stack for 'ecrecover' staticcall 
    0x20 
    0x00 
    0x80 
    0x00 
    chainid 
    gas
    staticcall validate jumpi

    oog jump

    /// Check if caller==retdata (signer address)
    validate:
    0x00 mload 
    dup1
    caller 
    eq valid jumpi

    oog jump

    // Return true
    valid: 
    chainid 
    0x00 mstore
    0x20 0x00 return

    // out-of-gas
    oog:
    0x01 
    0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff 
    mstore
}
```

### Breakdown of the Contract

#### Checking Calldata Size

The first part of the contract ensures that the calldata size is 97 bytes, which is the combined size of a 32-byte message hash and a 65-byte signature (v, r, s).

```huff
calldatasize 
0x61 eq 
extractParamsAndStore jumpi

oog jump
```

- `calldatasize`: Gets the size of the calldata.
- `0x61 eq`: Checks if the size equals 97 (0x61 in hex).
- `extractParamsAndStore jumpi`: Jumps to the `extractParamsAndStore` label if the size is correct.
- `oog jump`: Jumps to the `oog` label if the size is incorrect, causing the transaction to run out of gas.

#### Extracting Parameters

Next, we extract the message hash and the signature components (v, r, s) from the calldata and store them in memory.

```huff
extractParamsAndStore:
/// Store the message hash
0x00 calldataload
0x00 mstore

/// Store 'v'
0x60 calldataload
0x3f mstore

/// Store 'r'
0x20 calldataload
0x40 mstore 

/// Store 's'
0x40 calldataload
0x60 mstore 
```

- `calldataload`: Loads data from calldata at specified offset.
- `mstore`: Stores data in memory at specified offset.

#### Preparing for `ecrecover`

We prepare the stack with parameters for the `ecrecover` static call.

```huff
0x20 
0x00 
0x80 
0x00 
chainid 
gas
staticcall validate jumpi

oog jump
```

- `staticcall`: Performs a static call to the `ecrecover` precompiled contract.
- `validate jumpi`: Jumps to the `validate` label if the call is successful.
- `oog jump`: Jumps to the `oog` label if the call fails.

#### Validating the Signer

We verify if the extracted signer's address matches the transaction sender's address.

```huff
validate:
0x00 mload          // [rcvd_address]
dup1                // [rcvd_address, rcvd_address]
caller              // [msg.sender, rcvd_address]
eq valid jumpi      // [msg.sender == rcvd_address?]

oog jump            // if not equal, jump to out-of-gas block
```

- `mload`: Loads the signer's address from memory.
- `caller`: Gets the address of the transaction sender.
- `eq`: Checks if the two addresses are equal.
- `valid jumpi`: Jumps to the `valid` label if they are equal.
- `oog jump`: Jumps to the `oog` label if they are not equal.

#### Returning True

If the signature is valid, we return true.

```huff
valid: 
chainid 
0x00 mstore
0x20 0x00 return
```

- `chainid`: Gets the chain ID (used here to represent true).
- `mstore`: Stores the result in memory.
- `return`: Returns the result.

#### Out-of-Gas

If any check fails, we run out of gas.

```huff
oog:
0x01 
0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff 
mstore
```

- Attempts to store a massive value into memory, causing the transaction to run out of gas.

## Conclusion

This optimized Huff contract verifies if a message was signed by the sender of the transaction. By carefully structuring the calldata and using efficient opcodes, we ensure that the contract performs its task effectively. If any part of the validation fails, the contract will cause the transaction to run out of gas.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
