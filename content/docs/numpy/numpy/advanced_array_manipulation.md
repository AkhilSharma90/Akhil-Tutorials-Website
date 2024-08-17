---
title: "Advanced Array Manipulation with NumPy"
description: "..."
icon: "🎲"
draft: false
---

### Introduction

NumPy is a powerful library for numerical computing in Python. While basic array operations are straightforward, advanced array manipulations can significantly optimize your code and enhance performance. This tutorial covers advanced techniques for manipulating arrays, including reshaping, stacking, splitting, broadcasting, vectorization, and using advanced indexing.

### 1. Reshaping Arrays

**1.1 Changing the Shape of an Array**

```python
import numpy as np

# Creating a 1D array
arr = np.arange(12)
print("Original array:\n", arr)

# Reshaping to a 3x4 array
reshaped_arr = arr.reshape(3, 4)
print("Reshaped to 3x4 array:\n", reshaped_arr)
```

**1.2 Flattening an Array**

```python
# Flattening the array back to 1D
flattened_arr = reshaped_arr.ravel()
print("Flattened array:\n", flattened_arr)
```

**1.3 Resizing Arrays**

```python
# Resizing the array (changes original array)
arr.resize(2, 6)
print("Resized array:\n", arr)
```

### 2. Stacking and Splitting Arrays

**2.1 Stacking Arrays**

```python
# Creating two 2x2 arrays
A = np.array([[1, 2], [3, 4]])
B = np.array([[5, 6], [7, 8]])

# Vertical stacking
vstacked = np.vstack((A, B))
print("Vertically stacked array:\n", vstacked)

# Horizontal stacking
hstacked = np.hstack((A, B))
print("Horizontally stacked array:\n", hstacked)
```

**2.2 Splitting Arrays**

```python
# Splitting the array vertically into 2 arrays
vsplit = np.vsplit(vstacked, 2)
print("Vertically split arrays:", vsplit)

# Splitting the array horizontally into 2 arrays
hsplit = np.hsplit(hstacked, 2)
print("Horizontally split arrays:", hsplit)
```

### 3. Broadcasting

**3.1 Basic Broadcasting**

```python
# Creating a 3x1 array
x = np.array([[1], [2], [3]])

# Broadcasting with a 1x3 array
y = np.array([4, 5, 6])

# Broadcasting and addition
result = x + y
print("Broadcasted addition result:\n", result)
```

**3.2 Broadcasting Rules**

1. If the arrays do not have the same rank, prepend the shape of the lower-rank array with ones.
2. The two arrays are said to be compatible if they have the same shape or if one of the dimensions is 1.
3. The arrays can be broadcast together if they are compatible.

### 4. Vectorization

**4.1 Vectorized Operations**

```python
# Creating an array of numbers from 0 to 9
arr = np.arange(10)

# Vectorized addition
vectorized_result = arr + 2
print("Vectorized addition result:\n", vectorized_result)

# Vectorized condition
condition_result = arr[arr % 2 == 0]
print("Elements satisfying the condition (even numbers):\n", condition_result)
```

**4.2 Performance Comparison**

```python
# Using a loop
loop_result = []
for i in arr:
    loop_result.append(i + 2)
loop_result = np.array(loop_result)
print("Loop result:\n", loop_result)

# Vectorized operation is typically much faster
```

### 5. Advanced Indexing and Slicing

**5.1 Boolean Indexing**

```python
# Boolean indexing to select elements
bool_idx = arr > 5
print("Boolean index array:\n", bool_idx)

selected_elements = arr[bool_idx]
print("Selected elements:\n", selected_elements)
```

**5.2 Fancy Indexing**

```python
# Creating an array
arr = np.arange(10, 100, 10)
print("Original array:\n", arr)

# Fancy indexing with a list of indices
fancy_idx = [1, 3, 5]
selected_elements = arr[fancy_idx]
print("Selected elements using fancy indexing:\n", selected_elements)
```

**5.3 Multi-dimensional Indexing**

```python
# Creating a 3x3 array
arr = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])

# Indexing with an array of row indices and column indices
row_idx = np.array([0, 1, 2])
col_idx = np.array([2, 1, 0])

selected_elements = arr[row_idx, col_idx]
print("Selected elements using multi-dimensional indexing:\n", selected_elements)
```

### 6. Using `np.where` and `np.select`

**6.1 Conditional Selection with `np.where`**

```python
# Using np.where to replace elements based on a condition
arr = np.arange(10)
replaced_arr = np.where(arr % 2 == 0, 'even', 'odd')
print("Array after conditional replacement:\n", replaced_arr)
```

**6.2 Multiple Conditions with `np.select`**

```python
# Using np.select for multiple conditions
conditions = [
    (arr < 5),
    (arr >= 5) & (arr < 8),
    (arr >= 8)
]
choices = ['low', 'medium', 'high']
selected_arr = np.select(conditions, choices)
print("Array after multiple conditional selection:\n", selected_arr)
```

### Conclusion

In this tutorial, we've explored advanced array manipulation techniques using NumPy, including reshaping, stacking, splitting, broadcasting, vectorization, and advanced indexing. These tools and techniques are essential for efficient numerical computing and can greatly enhance the performance and readability of your code.

This comprehensive tutorial should provide a solid understanding of advanced array manipulation techniques in NumPy, making your numerical computing tasks more efficient and effective.

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
