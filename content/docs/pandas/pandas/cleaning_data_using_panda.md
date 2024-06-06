---
title: "How to Clean Data using Pandas"
description: "This section will focus on data cleaning, a pre-processing technique use in Machine Learning."
icon: "P"
draft: false
---

### Cleaning Data Using Pandas

Data cleaning is one of the most common tasks in data science. Pandas lets you preprocess data for various uses, including training machine learning and deep learning models. Let’s use the DataFrame `df2` from earlier, having four missing values, to illustrate a few data cleaning use cases. As a reminder, here's how you can see how many missing values are in a DataFrame.

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

#### Dealing with Missing Data Technique #1: Dropping Missing Values

One way to deal with missing data is to drop it. This is useful when you have plenty of data and losing a small portion won’t impact the downstream analysis. You can use the `.dropna()` method as shown below. Here, we are saving the results from `.dropna()` into a DataFrame `df3`.

```python
df3 = df2.copy()
df3 = df3.dropna()
df3.shape
```

```
(764, 9) # this is 4 rows less than df2
```

The `axis` argument lets you specify whether you are dropping rows or columns with missing values. The default axis removes the rows containing NaNs. Use `axis=1` to remove the columns with one or more NaN values. Also, notice how we are using the argument `inplace=True` which lets you skip saving the output of `.dropna()` into a new DataFrame.

```python
df3 = df2.copy()
df3.dropna(inplace=True, axis=1)
df3.head()
```

_Dropping missing data in pandas_

You can also drop both rows and columns with missing values by setting the `how` argument to 'all'.

```python
df3 = df2.copy()
df3.dropna(inplace=True, how='all')
```

#### Dealing with Missing Data Technique #2: Replacing Missing Values

Instead of dropping, replacing missing values with a summary statistic or a specific value (depending on the use case) may be the best approach. For example, if there is one missing row from a temperature column denoting temperatures throughout the days of the week, replacing that missing value with the average temperature of that week may be more effective than dropping values completely. You can replace the missing data with the row or column mean using the code below.

```python
df3 = df2.copy()
# Get the mean of Pregnancies
mean_value = df3['Pregnancies'].mean()
# Fill missing values using .fillna()
df3 = df3.fillna(mean_value)
```

#### Dealing with Duplicate Data

Let's add some duplicates to the original data to learn how to eliminate duplicates in a DataFrame. Here, we are using the `.concat()` method to concatenate the rows of the `df2` DataFrame to the `df2` DataFrame, adding perfect duplicates of every row in `df2`.

```python
df3 = pd.concat([df2, df2])
df3.shape
```

```
(1536, 9)
```

You can remove all duplicate rows (default) from the DataFrame using the `.drop_duplicates()` method.

```python
df3 = df3.drop_duplicates()
df3.shape
```

```
(768, 9)
```

#### Renaming Columns

A common data cleaning task is renaming columns. With the `.rename()` method, you can use `columns` as an argument to rename specific columns. The below code shows the dictionary for mapping old and new column names.

```python
df3.rename(columns={'DiabetesPedigreeFunction': 'DPF'}, inplace=True)
df3.head()
```

_Renaming columns in pandas_

You can also directly assign column names as a list to the DataFrame.

```python
df3.columns = ['Glucose', 'BloodPressure', 'SkinThickness', 'Insulin', 'BMI', 'DPF', 'Age', 'Outcome', 'STF']
df3.head()
```

_Renaming columns in pandas_

For more on data cleaning, and for easier, more predictable data cleaning workflows, check out the following checklist, which provides you with a comprehensive set of common data cleaning tasks.

### Data Analysis in Pandas

The main value proposition of pandas lies in its quick data analysis functionality. In this section, we'll focus on a set of analysis techniques you can use in pandas.

#### Summary Operators (Mean, Mode, Median)

As you saw earlier, you can get the mean of each column value using the `.mean()` method.

```python
df.mean()
```

_Printing the mean of columns in pandas_

A mode can be computed similarly using the `.mode()` method.

```python
df.mode()
```

_Printing the mode of columns in pandas_

Similarly, the median of each column is computed with the `.median()` method.

```python
df.median()
```

_Printing the median of columns in pandas_

#### Create New Columns Based on Existing Columns

Pandas provides fast and efficient computation by combining two or more columns like scalar variables. The below code divides each value in the column `Glucose` by the corresponding value in the `Insulin` column to compute a new column named `Glucose_Insulin_Ratio`.

```python
df2['Glucose_Insulin_Ratio'] = df2['Glucose'] / df2['Insulin']
df2.head()
```

_Create a new column from existing columns in pandas_

#### Counting Using `.value_counts()`

Often, you'll work with categorical values, and you'll want to count the number of observations each category has in a column. Category values can be counted using the `.value_counts()` method. Here, for example, we are counting the number of observations where `Outcome` is diabetic (1) and the number of observations where the `Outcome` is non-diabetic (0).

```python
df['Outcome'].value_counts()
```

_Using `.value_counts()` in pandas_

Adding the `normalize` argument returns proportions instead of absolute counts.

```python
df['Outcome'].value_counts(normalize=True)
```

_Using `.value_counts()` in pandas with normalization_

Turn off automatic sorting of results using the `sort` argument (True by default). The default sorting is based on the counts in descending order.

```python
df['Outcome'].value_counts(sort=False)
```

_Using `.value_counts()` in pandas with sorting_

You can also apply `.value_counts()` to a DataFrame object and specific columns within it instead of just a column. Here, for example, we are applying `value_counts()` on `df` with the `subset` argument, which takes in a list of columns.

```python
df.value_counts(subset=['Pregnancies', 'Outcome'])
```

_Using `.value_counts()` in pandas while subsetting columns_

#### Aggregating Data with `.groupby()` in Pandas

Pandas lets you aggregate values by grouping them by specific column values. You can do that by combining the `.groupby()` method with a summary method of your choice. The below code displays the mean of each of the numeric columns grouped by `Outcome`.

```python
df.groupby('Outcome').mean()
```

_Aggregating data by one column in pandas_

`.groupby()` enables grouping by more than one column by passing a list of column names, as shown below.

```python
df.groupby(['Pregnancies', 'Outcome']).mean()
```

_Aggregating data by two columns in pandas_

Any summary method can be used alongside `.groupby()`, including `.min()`, `.max()`, `.mean()`, `.median()`, `.sum()`, `.mode()`, and more.

#### Pivot Tables

Pandas also enables you to calculate summary statistics as pivot tables. This makes it easy to draw conclusions based on a combination of variables. The below code picks the rows as unique values of `Pregnancies`, the column values as the unique values of `Outcome`, and the cells contain the average value of `BMI` in the corresponding group.

For example, for `Pregnancies = 5` and `Outcome = 0`, the average BMI turns out to be 31.1.

```python
pd.pivot_table(df, values="BMI", index='Pregnancies', columns=['Outcome'], aggfunc=np.mean)
```

_Aggregating data by pivoting with pandas_

### Data Visualization in Pandas

Pandas provides convenience wrappers to Matplotlib plotting functions to make it easy to visualize your DataFrames. Below, you'll see how to do common data visualizations using pandas.

#### Line Plots in Pandas

Pandas enables you to chart out the relationships among variables using line plots. Below is a line plot of `BMI` and `Glucose` versus the row index.

```python
df[['BMI', 'Glucose']].plot.line()
```

_Basic line plot with pandas_

You can select the choice of colors by using the `color` argument.

```python
df[['BMI', 'Glucose']].plot.line(figsize=(20, 10), color={"BMI": "red", "Glucose": "blue"})
```

_Basic line plot with pandas, with custom colors_

All the columns of `df` can also be plotted on different scales and axes by using the `subplots` argument.

```python
df.plot.line(subplots=True)
```

_Subplots for line plots with pandas_

#### Bar Plots in Pandas

For discrete columns, you can use a bar plot over the category counts to visualize their distribution. The
