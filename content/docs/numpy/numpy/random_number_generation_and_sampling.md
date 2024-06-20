---
title: "Random Number Generation and Sampling with NumPy"
description: "..."
icon: "🎲"
draft: false
---

### Introduction
Random number generation and sampling are essential tasks in various scientific and computational applications. NumPy provides a powerful suite of functions for generating random numbers and sampling from different probability distributions. In this tutorial, we'll explore how to generate random numbers, create random arrays, and sample from different distributions using NumPy.

### 1. Generating Random Numbers

**1.1 Generating a Single Random Number**
```python
import numpy as np

# Generate a single random number between 0 and 1
random_number = np.random.rand()
print("Random number between 0 and 1:", random_number)
```

**1.2 Generating an Array of Random Numbers**
```python
# Generate an array of random numbers of shape (3, 3) between 0 and 1
random_array = np.random.rand(3, 3)
print("Array of random numbers between 0 and 1:\n", random_array)
```

**1.3 Generating Random Integers**
```python
# Generate a random integer between 1 and 10
random_int = np.random.randint(1, 11)
print("Random integer between 1 and 10:", random_int)
```

### 2. Sampling from Probability Distributions

**2.1 Uniform Distribution**
```python
# Sample 5 random numbers from a uniform distribution between 0 and 1
uniform_samples = np.random.uniform(0, 1, 5)
print("Uniformly distributed random samples:", uniform_samples)
```

**2.2 Normal Distribution**
```python
# Sample 5 random numbers from a normal distribution with mean 0 and standard deviation 1
normal_samples = np.random.normal(0, 1, 5)
print("Normally distributed random samples:", normal_samples)
```

**2.3 Binomial Distribution**
```python
# Sample 5 random numbers from a binomial distribution with 10 trials and probability of success 0.5
binomial_samples = np.random.binomial(10, 0.5, 5)
print("Binomially distributed random samples:", binomial_samples)
```

**2.4 Custom Distribution**
```python
# Sample 5 random numbers from a custom distribution
custom_distribution = [0.1, 0.2, 0.3, 0.4, 0.5]
custom_samples = np.random.choice(custom_distribution, 5)
print("Custom distributed random samples:", custom_samples)
```

### 3. Seeding Randomness

```python
# Seed the random number generator for reproducibility
np.random.seed(42)

# Generate random numbers after seeding
random_array = np.random.rand(3, 3)
print("Random array with seed 42:\n", random_array)
```

### Conclusion
Random number generation and sampling are crucial components of many computational tasks. NumPy's random module provides a wide range of functions for generating random numbers and sampling from various probability distributions. By understanding these functions and their parameters, you can generate random data tailored to your specific needs, enabling you to perform statistical simulations, model testing, and more with ease.

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).