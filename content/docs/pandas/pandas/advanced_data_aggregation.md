---
title: "Advanced Data Aggregation and Grouping in Pandas"
description: ""
icon: "K"
draft: false
---

Data aggregation and grouping are essential techniques for data analysis, allowing you to summarize and transform your data in various ways. This tutorial covers:

- Grouping data
- Applying aggregation functions
- Performing custom aggregations
- Handling missing values
- Multi-level grouping and aggregation

## Creating a Sample DataFrame

We'll start by creating a sample DataFrame to demonstrate these concepts.

```python
import pandas as pd

# Create sample DataFrame
data = {
    'Region': ['East', 'West', 'East', 'West', 'East', 'West'],
    'Category': ['Electronics', 'Electronics', 'Clothing', 'Clothing', 'Electronics', 'Clothing'],
    'Sales': [1000, 1500, 1200, 800, 900, 1100],
    'Price': [500, 600, 300, 400, 450, 350],
    'Quantity': [2, 3, 4, 2, 3, 4]
}
df = pd.DataFrame(data)
print("Sample DataFrame:")
print(df)
```

## Grouping Data

To group data, use the `groupby()` method. This example groups the data by `'Region'` and `'Category'`.

```python
# Grouping data by 'Region' and 'Category'
grouped = df.groupby(['Region', 'Category'])
```

## Aggregating Data

Aggregation functions such as `sum()`, `mean()`, and `max()` can be applied to grouped data.

### Single Aggregation Functions

```python
# Aggregating data within groups
agg_result = grouped.agg({
    'Sales': 'sum',
    'Price': 'mean',
    'Quantity': 'max'
})

print("Aggregated Data:")
print(agg_result)
```

### Multiple Aggregation Functions

You can apply multiple aggregation functions to each column.

```python
# Applying multiple aggregations
multi_agg_result = grouped.agg({
    'Sales': ['sum', 'mean'],
    'Price': ['min', 'max'],
    'Quantity': 'sum'
})

print("\nMultiple Aggregations:")
print(multi_agg_result)
```

## Handling Missing Values

To handle missing values during aggregation, you can use the `fillna()` method or specify how to handle `NaN` values directly in the aggregation functions.

```python
# Creating a DataFrame with missing values
data_with_nan = {
    'Region': ['East', 'West', 'East', 'West', 'East', 'West'],
    'Category': ['Electronics', 'Electronics', 'Clothing', 'Clothing', 'Electronics', 'Clothing'],
    'Sales': [1000, 1500, 1200, 800, None, 1100],
    'Price': [500, 600, 300, 400, 450, None],
    'Quantity': [2, 3, 4, 2, 3, 4]
}
df_nan = pd.DataFrame(data_with_nan)

# Filling missing values
df_nan_filled = df_nan.fillna(0)
grouped_nan = df_nan_filled.groupby(['Region', 'Category']).sum()
print("\nData with Missing Values Handled:")
print(grouped_nan)
```

## Custom Aggregation Functions

You can create custom aggregation functions and apply them using `agg()`.

```python
# Custom aggregation function
def range_values(series):
    return series.max() - series.min()

custom_agg_result = grouped.agg({
    'Sales': range_values,
    'Price': range_values
})

print("\nCustom Aggregation:")
print(custom_agg_result)
```

## Accessing Individual Groups

You can iterate through each group and access its data.

```python
# Grouping by multiple columns
multi_column_grouped = df.groupby(['Region', 'Category'])

# Accessing individual groups
for name, group in multi_column_grouped:
    print(f"\nGroup: {name}")
    print(group)
```

## Multi-level Grouping and Aggregation

Pandas supports multi-level (hierarchical) grouping and aggregation.

```python
# Aggregating with multi-level grouping
multi_level_agg = df.groupby(['Region', 'Category']).agg({
    'Sales': ['sum', 'mean'],
    'Price': ['min', 'max'],
    'Quantity': 'sum'
})

print("\nMulti-level Aggregation:")
print(multi_level_agg)
```

## Conclusion

This tutorial covers advanced data aggregation and grouping techniques in Pandas. By mastering these techniques, you can perform complex data analyses and derive meaningful insights from your data. Experiment with different datasets and aggregation functions to further enhance your skills.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).