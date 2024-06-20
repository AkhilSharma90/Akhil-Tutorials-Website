---
title: "Extending Pandas with PyArrow: Enhanced Functionality and Performance"
description: ""
icon: "K"
draft: false
---

Pandas, the go-to data manipulation library in Python, can further extend its capabilities and improve performance by leveraging PyArrow. PyArrow provides a robust interface for working with Apache Arrow, a columnar in-memory data format optimized for analytics. In this blog, we'll explore how Pandas integrates with PyArrow to offer more extensive data types, improved support for missing data, performant IO operations, and interoperability with other data frame libraries.

## PyArrow Functionality in Pandas

PyArrow enhances Pandas' functionality in several key areas:

1. **More Extensive Data Types Compared to NumPy**: PyArrow supports a broader range of data types, including more complex types such as decimal and map.
2. **Missing Data Support (NA) for All Data Types**: Unlike NumPy, PyArrow provides consistent missing data (NA) support across all data types.
3. **Performant IO Reader Integration**: PyArrow can significantly accelerate IO operations such as reading from CSV or JSON files.
4. **Interoperability with Other DataFrame Libraries**: Based on the Apache Arrow specification, PyArrow facilitates seamless interoperability with other libraries like Polars and cuDF.

### Minimum Supported PyArrow Version

To take advantage of these features, ensure you have the minimum supported version of PyArrow installed. You can install PyArrow using pip:

```bash
pip install pyarrow
```

## Data Structure Integration

### Creating PyArrow-Backed Pandas Objects

Pandas allows you to create Series, Index, or DataFrame columns backed by PyArrow. This is achieved by specifying the data type using the `dtype` parameter.

#### Example: Creating a Series with PyArrow

```python
import pandas as pd

ser = pd.Series([-1.5, 0.2, None], dtype="float32[pyarrow]")
print(ser)
```

Output:
```
0    -1.5
1     0.2
2    <NA>
dtype: float[pyarrow]
```

#### Example: Creating an Index with PyArrow

```python
idx = pd.Index([True, None], dtype="bool[pyarrow]")
print(idx)
```

Output:
```
Index([True, <NA>], dtype='bool[pyarrow]')
```

#### Example: Creating a DataFrame with PyArrow

```python
df = pd.DataFrame([[1, 2], [3, 4]], dtype="uint64[pyarrow]")
print(df)
```

Output:
```
   0  1
0  1  2
1  3  4
```

### Differences Between `string[pyarrow]` and `pd.ArrowDtype(pa.string())`

There are subtle differences between the string alias `"string[pyarrow]"` and specifying `dtype=pd.ArrowDtype(pa.string())`. Generally, operations on data will behave similarly, but some differences in return types exist.

```python
import pyarrow as pa
import pandas as pd

data = list("abc")

ser_sd = pd.Series(data, dtype="string[pyarrow]")
ser_ad = pd.Series(data, dtype=pd.ArrowDtype(pa.string()))

print(ser_ad.dtype == ser_sd.dtype)  # Output: False

print(ser_sd.str.contains("a"))
print(ser_ad.str.contains("a"))
```

Output:
```
False
0     True
1    False
2    False
dtype: boolean
0     True
1    False
2    False
dtype: bool[pyarrow]
```

### Handling More Complex Data Types

For PyArrow types that accept parameters, you can pass a PyArrow type with those parameters into `ArrowDtype`.

#### Example: List of Strings

```python
list_str_type = pa.list_(pa.string())
ser = pd.Series([["hello"], ["there"]], dtype=pd.ArrowDtype(list_str_type))
print(ser)
```

Output:
```
0    ['hello']
1    ['there']
dtype: list<item: string>[pyarrow]
```

#### Example: Time and Decimal Types

```python
from datetime import time
from decimal import Decimal

# Time Type
time_type = pd.ArrowDtype(pa.time64("us"))
idx = pd.Index([time(12, 30), None], dtype=time_type)
print(idx)

# Decimal Type
decimal_type = pd.ArrowDtype(pa.decimal128(3, scale=2))
data = [[Decimal("3.19"), None], [None, Decimal("-1.23")]]
df = pd.DataFrame(data, dtype=decimal_type)
print(df)
```

Output:
```
Index([12:30:00, <NA>], dtype='time64[us][pyarrow]')
      0      1
0  3.19   <NA>
1  <NA>  -1.23
```

### Working with PyArrow Arrays and ChunkedArrays

If you already have a PyArrow `Array` or `ChunkedArray`, you can construct the associated Pandas objects directly.

#### Example: Creating a Series from a PyArrow Array

```python
pa_array = pa.array([{"1": "2"}, {"10": "20"}, None], type=pa.map_(pa.string(), pa.string()))
ser = pd.Series(pd.arrays.ArrowExtensionArray(pa_array))
print(ser)
```

Output:
```
0      [('1', '2')]
1    [('10', '20')]
2              <NA>
dtype: map<string, string>[pyarrow]
```

### Retrieving PyArrow Arrays from Pandas Objects

You can retrieve a PyArrow `ChunkedArray` from a Pandas Series or Index.

#### Example: Retrieving PyArrow Array

```python
ser = pd.Series([1, 2, None], dtype="uint8[pyarrow]")
print(pa.array(ser))

idx = pd.Index(ser)
print(pa.array(idx))
```

Output:
```
<pyarrow.lib.UInt8Array object at 0x7ff2a2968400>
[
  1,
  2,
  null
]
<pyarrow.lib.UInt8Array object at 0x7ff2a2968460>
[
  1,
  2,
  null
]
```

### Converting a PyArrow Table to a DataFrame

You can convert a PyArrow `Table` to a Pandas DataFrame using the `to_pandas()` method.

#### Example: Converting PyArrow Table

```python
table = pa.table([pa.array([1, 2, 3], type=pa.int64())], names=["a"])
df = table.to_pandas(types_mapper=pd.ArrowDtype)
print(df)
print(df.dtypes)
```

Output:
```
   a
0  1
1  2
2  3
a    int64[pyarrow]
dtype: object
```

## PyArrow-Accelerated Operations

Pandas integrates PyArrow to accelerate several operations, including numeric aggregations, arithmetic, logical operations, and more.

#### Examples of Accelerated Operations

```python
import pyarrow as pa

ser = pd.Series([-1.545, 0.211, None], dtype="float32[pyarrow]")
print(ser.mean())  # Output: -0.6669999808073044

print(ser + ser)  # Output: 0 -3.09, 1 0.422, 2 <NA>

print(ser > (ser + 1))  # Output: 0 False, 1 False, 2 <NA>

print(ser.dropna())  # Output: 0 -1.545, 1 0.211

print(ser.isna())  # Output: 0 False, 1 False, 2 True

print(ser.fillna(0))  # Output: 0 -1.545, 1 0.211, 2 0.0
```

### String Operations

```python
ser_str = pd.Series(["a", "b", None], dtype=pd.ArrowDtype(pa.string()))
print(ser_str.str.startswith("a"))
```

Output:
```
0     True
1    False
2     <NA>
dtype: bool[pyarrow]
```

### Datetime Operations

```python
from datetime import datetime

pa_type = pd.ArrowDtype(pa.timestamp("ns"))
ser_dt = pd.Series([datetime(2022, 1, 1), None], dtype=pa_type)
print(ser_dt.dt.strftime("%Y-%m"))
```

Output:
```
0    2022-01
1       <NA>
dtype: string[pyarrow]
```

## PyArrow-Accelerated IO Reading

PyArrow can be used to speed up various IO operations in Pandas, such as reading from CSV or JSON files. You can specify the `engine="pyarrow"` parameter to utilize PyArrow's capabilities.

#### Example: Reading CSV with PyArrow

```python
import io

data = io.StringIO("""a,b,c
1,2.5,True
3,4.5,False
""")
df = pd.read_csv(data, engine="pyarrow")
print(df)
```

Output:
```
   a    b      c
0  1  2.5   True
1  3  4.5  False
```

### Returning PyArrow-Backed Data

To return PyArrow-backed data, use the `dtype_backend="pyarrow"` parameter.

#### Example: Returning PyArrow-Backed Data

```python
data = io.StringIO("""a,b,c,d,e,f,g,h,i
1

,2.5,True,a,,,,,
3,4.5,False,b,6,7.5,True,a,
""")
df_pyarrow = pd.read_csv(data, dtype_backend="pyarrow")
print(df_pyarrow.dtypes)
```

Output:
```
a     int64[pyarrow]
b    double[pyarrow]
c      bool[pyarrow]
d    string[pyarrow]
e     int64[pyarrow]
f    double[pyarrow]
g      bool[pyarrow]
h    string[pyarrow]
i      null[pyarrow]
dtype: object
```

## Conclusion

Integrating PyArrow with Pandas extends the library's functionality, improves performance, and enables more complex data manipulations. By leveraging PyArrow, you can handle a wider range of data types, achieve better missing data support, and accelerate various operations and IO tasks. Ensure you have PyArrow installed and explore the enhanced capabilities it brings to your Pandas workflows.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).