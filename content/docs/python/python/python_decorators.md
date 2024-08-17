---
title: "Demystifying Python Decorators: Enhancing Functionality with Decorators"
description: "Unlock the power of decorators in Python to modify and enhance the functionality of functions and methods dynamically. This guide delves into the principles of decorators and shows you how to create them with practical, real-world examples."
draft: false
---

## Introduction

Decorators in Python are a very powerful and useful tool, allowing programmers to modify the behavior of a function or class. Decorators are typically used to extend or alter the behavior of functions or methods without permanently modifying them. They provide a flexible way to "wrap" functions with additional code.

### What are Decorators?

A decorator in Python is essentially a function that takes another function and extends its functionality, often doing some processing before or after the execution of the original function.

#### Basic Concept of a Decorator

```python
def decorator(func):
    def wrapper():
        print("Something is happening before the function is called.")
        func()
        print("Something is happening after the function is called.")
    return wrapper

def say_hello():
    print("Hello!")

# Apply the decorator
say_hello = decorator(say_hello)
say_hello()
```

In this example, `decorator` is a function that takes another function `func` as an argument. The `wrapper` function is defined inside the decorator and wraps the functionality of the `func` function by adding some code before and after its call.

### Using the `@` Syntax for Decorators

Python provides a simpler way to apply decorators using the `@` symbol, which is placed above the definition of the function to be decorated.

#### Example Using `@`

```python
@decorator
def say_goodbye():
    print("Goodbye!")

say_goodbye()
```

The `@decorator` syntax is just a shorthand for `say_goodbye = decorator(say_goodbye)`, making the code cleaner and more readable.

### Building a Simple Decorator

Let’s create a simple decorator that logs the execution time of any function it decorates.

#### Execution Time Decorator

```python
import time

def timer(func):
    def wrapper(*args, **kwargs):
        start_time = time.time()
        result = func(*args, **kwargs)
        end_time = time.time()
        print(f"Executing {func.__name__} took {end_time - start_time} seconds.")
        return result
    return wrapper

@timer
def long_running_task():
    for _ in range(1000000):
        pass

long_running_task()
```

This `timer` decorator measures the time it takes to execute the function `long_running_task`. The `wrapper` function uses `*args` and `**kwargs` to handle any number of arguments passed to the function. It calculates the start time and end time around the function call, and then prints the duration.

### Conclusion

Decorators are a valuable feature in Python, offering an elegant and expressive way to modify functions' behavior dynamically. Understanding and utilizing decorators can lead to cleaner, more efficient, and more maintainable code, especially in large-scale applications. This guide has introduced the concept of decorators, shown how to apply them, and demonstrated how to create a practical decorator for measuring execution time.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
