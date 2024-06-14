---
title: "A simple Huff Program to Determine Even Number"
description: "Huff is a domain-specific, low-level programming language designed explicitly for writing smart contracts on the Ethereum blockchain."
icon: "code"
draft: false
---

In this tutorial, we are writing a Huff smart contract that checks whether a given number is even or odd. The contract should return 1 if the number is even and 0 if the number is odd.

**Note:** For those unfamiliar with calldata, it is a type of input data sent with a transaction. Calldata is stored outside the EVM's storage and memory, making it cheaper to use.

### Basic Solution

Here’s a basic Huff contract to solve this problem:

```huff
#define macro MAIN() = takes(0) returns (0) {
    0x02                       //[0x02]
    0x00 calldataload          //[input, 0x02]
    mod                        //[0 or 1]
    iszero                     //[1 or 0]
    0x00 mstore
    0x20 0x00 return
}
```

### Explanation

#### MAIN Macro

In Huff, execution always starts from the `MAIN` macro. The `takes(0) returns(0)` indicates that this macro doesn't read any values from the stack and doesn't push any value to the stack upon completion.

#### Logic

1. **Push 2 to the Stack**

   ```huff
   0x02
   ```

   This pushes the number 2 onto the stack.

2. **Load Calldata**

   ```huff
   0x00 calldataload
   ```

   The `calldataload` opcode loads the transaction input data to the stack. The opcode takes one argument, the offset (0x00) to start loading from.

3. **Modulus Operation**

   ```huff
   mod
   ```

   The `mod` opcode takes two inputs from the stack and returns the remainder of the division. If the number is even, the result will be 0.

4. **Check if Zero**

   ```huff
   iszero
   ```

   The `iszero` opcode checks if the value at the top of the stack is zero. If it is, it returns 1 (indicating the number is even); otherwise, it returns 0 (indicating the number is odd).

5. **Store Result in Memory**

   ```huff
   0x00 mstore
   ```

   The `mstore` opcode stores the result at memory offset 0x00.

6. **Return Result**

   ```huff
   0x20 0x00 return
   ```

   The `return` opcode returns 32 bytes from memory offset 0x00.

### Optimization

While the above code does the job, there is room to save gas. Each time we push 0 onto the stack using `0x00`, Huff replaces it with the `PUSH` opcode, which costs 3 gas.

The EVM wizards in the Huff Discord found another way to push 0 onto the stack using the `RETURNDATASIZE` opcode, which costs only 2 gas. The `RETURNDATASIZE` opcode pushes the length of the data returned in the last call. Since we haven't made any external calls, it will push 0 onto the stack.

### Optimized Solution

Here’s the optimized Huff contract:

```huff
#define macro MAIN() = takes(0) returns (0) {
    0x02                                 //[0x02]
    returndatasize calldataload          //[input, 0x02]
    mod                                  //[0 or 1]
    iszero
    returndatasize mstore
    0x20 returndatasize return
}
```

#### Optimization Steps

1. **Push 2 to the Stack**

   ```huff
   0x02
   ```

2. **Load Calldata with Optimized 0**

   ```huff
   returndatasize calldataload
   ```

   This pushes 0 onto the stack using `RETURNDATASIZE`, saving 1 gas compared to using `0x00`.

3. **Modulus Operation**

   ```huff
   mod
   ```

4. **Check if Zero**

   ```huff
   iszero
   ```

5. **Store Result in Memory with Optimized 0**

   ```huff
   returndatasize mstore
   ```

6. **Return Result with Optimized 0**

   ```huff
   0x20 returndatasize return
   ```

By replacing `0x00` with `RETURNDATASIZE` in three places, we save 3 gas, equivalent to one `PUSH`.

## Conclusion

This optimized Huff contract efficiently determines whether a given number is even or odd. By leveraging the `RETURNDATASIZE` opcode, we reduce the gas consumption, making the contract more efficient.
