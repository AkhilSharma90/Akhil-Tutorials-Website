---
title: "Introduction to Data Structures in Pandas"
description: "This is an introduction to data structures in pandas, a python package for data analysis."
icon: "P"
draft: false
---
# 

`Pandas` is a powerful and widely-used data manipulation library in Python, providing versatile data structures and functions designed to make data analysis and manipulation simple and efficient. This blog will introduce you to two primary data structures in Pandas: Series and DataFrame. Understanding these data structures is fundamental to harnessing the full potential of Pandas for data analysis.

## Series

A Series is a one-dimensional labeled array capable of holding any data type (integers, strings, floating point numbers, Python objects, etc.). It is similar to a column in a spreadsheet or a database table. Let’s dive into some key characteristics and functionalities of Series.

### Series is ndarray-like

A Pandas Series is built on top of NumPy’s ndarray. This means that a Series inherits many of the capabilities of ndarray, such as element-wise operations and array manipulations.

```python
import pandas as pd
import numpy as np

data = np.array([1, 2, 3, 4])
s = pd.Series(data)
print(s)
```

Output:
```
0    1
1    2
2    3
3    4
dtype: int64
```

### Series is dict-like

A Series can also be thought of as a fixed-size, ordered dictionary. It is an ideal structure for working with time series or other labeled data. You can access elements using labels (indexes) just like you would in a dictionary.

```python
data = {'a': 1, 'b': 2, 'c': 3}
s = pd.Series(data)
print(s)
print(s['a'])
```

Output:
```
a    1
b    2
c    3
dtype: int64
1
```

### Vectorized Operations and Label Alignment with Series

Pandas Series support vectorized operations, which allow you to perform operations on entire arrays without writing explicit loops. This feature leverages the speed of NumPy operations. Additionally, Series automatically aligns data based on the labels during arithmetic operations.

```python
s1 = pd.Series([1, 2, 3], index=['a', 'b', 'c'])
s2 = pd.Series([4, 5, 6], index=['a', 'b', 'd'])
s = s1 + s2
print(s)
```

Output:
```
a    5.0
b    7.0
c    NaN
d    NaN
dtype: float64
```

### Name Attribute

The `name` attribute in Series can be used to assign a name to the Series object or its index, which can be useful for debugging and keeping track of data in larger datasets.

```python
s = pd.Series([1, 2, 3], name="numbers")
print(s)
print(s.name)
```

Output:
```
0    1
1    2
2    3
Name: numbers, dtype: int64
numbers
```

## DataFrame

A DataFrame is a two-dimensional, size-mutable, and potentially heterogeneous tabular data structure with labeled axes (rows and columns). It is the most commonly used Pandas object. Let's explore various ways to create DataFrames and their functionalities.

### From Dict of Series or Dicts

You can create a DataFrame from a dictionary of Series or dictionaries. Each Series becomes a column in the DataFrame.

```python
data = {
    'col1': pd.Series([1, 2, 3]),
    'col2': pd.Series([4, 5, 6])
}
df = pd.DataFrame(data)
print(df)
```

Output:
```
   col1  col2
0     1     4
1     2     5
2     3     6
```

### From Dict of ndarrays / Lists

A DataFrame can also be created from a dictionary of NumPy arrays or lists. The arrays must be of the same length.

```python
data = {
    'col1': [1, 2, 3],
    'col2': [4, 5, 6]
}
df = pd.DataFrame(data)
print(df)
```

Output:
```
   col1  col2
0     1     4
1     2     5
2     3     6
```

### From Structured or Record Array

DataFrames can be constructed from structured or record arrays.

```python
data = np.array([(1, 'A'), (2, 'B'), (3, 'C')], dtype=[('num', 'i4'), ('letter', 'U1')])
df = pd.DataFrame(data)
print(df)
```

Output:
```
   num letter
0    1      A
1    2      B
2    3      C
```

### From a List of Dicts

Creating a DataFrame from a list of dictionaries is straightforward, with each dictionary representing a row.

```python
data = [
    {'a': 1, 'b': 2},
    {'a': 3, 'b': 4, 'c': 5}
]
df = pd.DataFrame(data)
print(df)
```

Output:
```
     a  b    c
0  1.0  2  NaN
1  3.0  4  5.0
```

### From a Dict of Tuples

DataFrames can also be created from dictionaries of tuples.

```python
data = {
    'col1': (1, 2, 3),
    'col2': (4, 5, 6)
}
df = pd.DataFrame(data)
print(df)
```

Output:
```
   col1  col2
0     1     4
1     2     5
2     3     6
```

### From a Series

Creating a DataFrame from a Series is possible and results in a single-column DataFrame.

```python
s = pd.Series([1, 2, 3], name="numbers")
df = pd.DataFrame(s)
print(df)
```

Output:
```
   numbers
0        1
1        2
2        3
```

### From a List of Namedtuples

You can also construct DataFrames from a list of namedtuples.

```python
from collections import namedtuple

Person = namedtuple('Person', 'name age')
data = [Person('Alice', 25), Person('Bob', 30)]
df = pd.DataFrame(data)
print(df)
```

Output:
```
    name  age
0  Alice   25
1    Bob   30
```

### From a List of Dataclasses

Similarly, DataFrames can be created from a list of dataclasses.

```python
from dataclasses import dataclass

@dataclass
class Person:
    name: str
    age: int

data = [Person('Alice', 25), Person('Bob', 30)]
df = pd.DataFrame(data)
print(df)
```

Output:
```
    name  age
0  Alice   25
1    Bob   30
```

### Alternate Constructors

Pandas offers a range of alternate constructors for DataFrames, such as from_records, from_items, etc., to accommodate various data formats and structures.

### Column Selection, Addition, Deletion

Selecting, adding, and deleting columns in a DataFrame are straightforward tasks.

```python
data = {'col1': [1, 2], 'col2': [3, 4]}
df = pd.DataFrame(data)

# Select column
print(df['col1'])

# Add column
df['col3'] = [5, 6]
print(df)

# Delete column
del df['col2']
print(df)
```

Output:
```
0    1
1    2
Name: col1, dtype: int64
   col1  col2  col3
0     1     3     5
1     2     4     6
   col1  col3
0     1     5
1     2     6
```

### Assigning New Columns in Method Chains

You can assign new columns while chaining methods using the `assign` method.

```python
df = df.assign(col4=lambda x: x['col1'] + x['col3'])
print(df)
```

Output:
```
   col1  col3  col4
0     1     5     6
1     2     6     8
```

### Indexing / Selection

DataFrames offer robust indexing and selection capabilities. You can use `.loc` for label-based indexing and `.iloc` for positional indexing.

```python
print(df.loc[0, 'col1'])
print(df.iloc[0, 0])
```

Output:
```
1
1
```

### Data Alignment and Arithmetic

DataFrames automatically align data during arithmetic operations, similar to Series.

```python
df1 = pd.DataFrame({'A': [1, 2]}, index=['a', 'b'])
df2 = pd.DataFrame({'B': [3, 4]}, index=['b', 'a'])
result = df1 + df2
print(result)
```

Output:
```
     A   B
a  NaN NaN
b  NaN NaN
```

### Transposing

Transposing a DataFrame swaps its rows and columns.

```python
print(df.T)
```

Output:
```
      0  1
col1  1  2
col3