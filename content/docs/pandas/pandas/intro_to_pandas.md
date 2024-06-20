---
title: "An Introduction to Pandas"
description: "This is an introduction to pandas, a python package for data analysis."
icon: "P"
draft: false
---

`pandas` is arguably the most important Python package for data analysis. It is the de facto standard package for data manipulation and exploratory data analysis. Its ability to read from and write to an extensive list of formats makes it a versatile tool for data science practitioners. Its data manipulation functions make it a highly accessible and practical tool for aggregating, analyzing, and cleaning data. 

In this introduction on how to learn pandas, we discussed the learning path you may take to master this package. This beginner-friendly tutorial will cover all the basic concepts and illustrate pandas' different functions. You can also check out our course on pandas Foundations for further details. 

This article is aimed at beginners with basic knowledge of Python and no prior experience with pandas to help you get started.

## What is pandas?
pandas is a data manipulation package in Python for tabular data. That is, data in the form of rows and columns, also known as DataFrames. Intuitively, you can think of a DataFrame as an Excel sheet. 

pandas’ functionality includes data transformations, like sorting rows and taking subsets, to calculating summary statistics such as the mean, reshaping DataFrames, and joining DataFrames together.

## Uses for pandas
pandas is used throughout the data analysis workflow. With pandas, you can:

- Import datasets from databases, spreadsheets, comma-separated values (CSV) files, and more.
- Clean datasets, for example, by dealing with missing values.
- Tidy datasets by reshaping their structure into a suitable format for analysis.
- Aggregate data by calculating summary statistics such as the mean of columns, correlation between them, and more.
- Visualize datasets and uncover insights.

## Key benefits of the pandas package
Undoubtedly, pandas is a powerful data manipulation tool packaged with several benefits, including:

- Made for Python: Python is the world's most popular language for machine learning and data science.
- Less verbose per unit operations: Code written in pandas is less verbose, requiring fewer lines of code to get the desired output. 
- Intuitive view of data: pandas offers exceptionally intuitive data representation that facilitates easier data understanding and analysis.
- Extensive feature set: It supports an extensive set of operations from exploratory data analysis, dealing with missing values, calculating statistics, visualizing univariate and bivariate data, and much more.
- Works with large data: pandas handles large data sets with ease. It offers speed and efficiency while working with datasets of the order of millions of records and hundreds of columns, depending on the machine.

## Installations
Installing pandas is clear: we are going to be using `pip` to install pandas, either on your terminal, notebook or google colab.
```bash
pip install pandas
```

## Working with pandas

**Importing data in pandas**
Firstly import the pandas Python package as shown below. When importing pandas, the most common alias for pandas is pd:
```python
import pandas as pd
```

**Importing CSV files**
Use `read_csv()` with the path to the CSV file to read a comma-separated values file.

```python
import pandas as pd
df = pd.read_csv("movies.csv") 
```

**Importing Text Files** 

Reading text files is similar to reading CSV files. The only nuance is that you need to specify a separator with the `sep` argument, as shown below. The `sep` argument refers to the symbol used to separate rows in a DataFrame. Common separators include comma (`sep=","`), whitespace (`sep="\s"`), tab (`sep="\t"`), and colon (`sep=":"`). Here, `\s` represents a single whitespace character.

```python
df = pd.read_csv("movies.txt", sep="\s")
```

**Importing Excel Files (Single Sheet)**

Reading Excel files (both XLS and XLSX) is as easy as using the `read_excel()` function, with the file path as an input.

```python
df = pd.read_excel('movies.xlsx')
```

You can also specify other arguments, such as `header` to specify which row becomes the DataFrame's header. The default value is 0, which denotes the first row as headers or column names. You can also specify column names as a list in the `names` argument. The `index_col` (default is None) argument can be used if the file contains a row index.

**Note:** In a pandas DataFrame or Series, the index is an identifier that points to the location of a row or column in a pandas DataFrame. The index labels the row or column and lets you access a specific row or column by using its index. A DataFrame’s row index can be a range (e.g., 0 to 303), a time series (dates or timestamps), a unique identifier (e.g., `movie_ID` in a movies table), or other types of data. For columns, it's usually a string (denoting the column name).

**Importing Excel Files (Multiple Sheets)**

Reading Excel files with multiple sheets is not much different. You just need to specify one additional argument, `sheet_name`, where you can either pass a string for the sheet name or an integer for the sheet position (note that Python uses 0-indexing, where the first sheet can be accessed with `sheet_name=0`).

```python
# Extracting the second sheet since Python uses 0-indexing
df = pd.read_excel('movies_multi.xlsx', sheet_name=1)
```

**Importing JSON Files**

Similar to the `read_csv()` function, you can use `read_json()` for JSON file types with the JSON file name as the argument. The below code reads a JSON file from disk and creates a DataFrame object `df`.

```python
df = pd.read_json("movies.json")
```

If you want to learn more about importing data with pandas, check out this cheat sheet on importing various file types with Python.
```

```markdown
### Outputting Data in Pandas

Just as pandas can import data from various file types, it also allows you to export data into various formats. This is useful when data is transformed using pandas and needs to be saved locally on your machine. Below is how to output pandas DataFrames into various formats.

#### Outputting a DataFrame into a CSV File
A pandas DataFrame (here we are using `df`) is saved as a CSV file using the `.to_csv()` method. The arguments include the filename with path and `index` – where `index=True` implies writing the DataFrame’s index.

```python
df.to_csv("movies_out.csv", index=False)
```

#### Outputting a DataFrame into a JSON File
Export a DataFrame object into a JSON file by calling the `.to_json()` method.

```python
df.to_json("movies_out.json")
```

**Note:** A JSON file stores a tabular object like a DataFrame as a key-value pair. Thus, you would observe repeating column headers in a JSON file.

#### Outputting a DataFrame into a Text File
As with writing DataFrames to CSV files, you can call `.to_csv()`. The only differences are that the output file format is `.txt`, and you need to specify a separator using the `sep` argument.

```python
df.to_csv('movies_out.txt', header=df.columns, index=None, sep=' ')
```

#### Outputting a DataFrame into an Excel File
Call `.to_excel()` from the DataFrame object to save it as a “.xls” or “.xlsx” file.

```python
df.to_excel("movies_out.xlsx", index=False)
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).