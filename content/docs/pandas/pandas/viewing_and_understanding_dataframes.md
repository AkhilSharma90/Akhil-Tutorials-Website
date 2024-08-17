---
title: "Viewing and Understanding Dataframes"
description: "In this section, you will be learning more about Dataframes, how to load data into one and how to perform operations."
icon: "P"
draft: false
---

### Viewing and Understanding DataFrames Using Pandas

After reading tabular data as a DataFrame, you would need to have a glimpse of the data. You can either view a small sample of the dataset or a summary of the data in the form of summary statistics.

#### How to View Data Using `.head()` and `.tail()`

You can view the first few or last few rows of a DataFrame using the `.head()` or `.tail()` methods, respectively. You can specify the number of rows through the `n` argument (the default value is 5).

```python
df.head()
```

_First five rows of the DataFrame (df) using `.head()`_

```python
df.tail(n=10)
```

_Last 10 rows of the DataFrame using `.tail()`_

#### Understanding Data Using `.describe()`

The `.describe()` method prints the summary statistics of all numeric columns, such as count, mean, standard deviation, range, and quartiles of numeric columns.

```python
df.describe()
```

_Get summary statistics with `.describe()`_

It gives a quick look at the scale, skew, and range of numeric data.

You can also modify the quartiles using the `percentiles` argument. Here, for example, we’re looking at the 30%, 50%, and 70% percentiles of the numeric columns in DataFrame `df`.

```python
df.describe(percentiles=[0.3, 0.5, 0.7])
```

_Get summary statistics with specific percentiles_

You can also isolate specific data types in your summary output by using the `include` argument. Here, for example, we’re only summarizing the columns with the integer data type.

```python
df.describe(include=[int])
```

_Get summary statistics of integer columns only_

Similarly, you might want to exclude certain data types using the `exclude` argument.

```python
df.describe(exclude=[int])
```

_Get summary statistics of non-integer columns only_

Often, practitioners find it easy to view such statistics by transposing them with the `.T` attribute.

```python
df.describe().T
```

_Transpose summary statistics with `.T`_

#### Understanding Data Using `.info()`

The `.info()` method is a quick way to look at the data types, missing values, and data size of a DataFrame. Here, we’re setting the `show_counts` argument to `True`, which gives an overview of the total non-missing values in each column. We’re also setting `memory_usage` to `True`, which shows the total memory usage of the DataFrame elements. When `verbose` is set to `True`, it prints the full summary from `.info()`.

```python
df.info(show_counts=True, memory_usage=True, verbose=True)
```

#### Understanding Your Data Using `.shape`

The number of rows and columns of a DataFrame can be identified using the `.shape` attribute of the DataFrame. It returns a tuple `(row, column)` and can be indexed to get only rows or only columns count as output.

```python
df.shape # Get the number of rows and columns
df.shape[0] # Get the number of rows only
df.shape[1] # Get the number of columns only
```

#### Get All Columns and Column Names

Calling the `.columns` attribute of a DataFrame object returns the column names in the form of an Index object. As a reminder, a pandas index is the address/label of the row or column.

```python
df.columns
```

Output of columns:

It can be converted to a list using the `list()` function.

```python
list(df.columns)
```

#### Checking for Missing Values in Pandas with `.isnull()`

The sample DataFrame does not have any missing values. Let's introduce a few to make things interesting. The `.copy()` method makes a copy of the original DataFrame. This is done to ensure that any changes to the copy don’t reflect in the original DataFrame. Using `.loc` (to be discussed later), you can set rows two to five of the `Pregnancies` column to `NaN` values, which denote missing values.

```python
df2 = df.copy()
df2.loc[2:5, 'Pregnancies'] = None
df2.head(7)
```

_Rows 2 to 5 are `NaN`_

You can check whether each element in a DataFrame is missing using the `.isnull()` method.

```python
df2.isnull().head(7)
```

Given it's often more useful to know how much missing data you have, you can combine `.isnull()` with `.sum()` to count the number of nulls in each column.

```python
df2.isnull().sum()
```

```
Pregnancies                 4
Glucose                     0
BloodPressure               0
SkinThickness               0
Insulin                     0
BMI                         0
DiabetesPedigreeFunction    0
Age                         0
Outcome                     0
dtype: int64
```

You can also do a double sum to get the total number of nulls in the DataFrame.

```python
df2.isnull().sum().sum()
```

```
4
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
