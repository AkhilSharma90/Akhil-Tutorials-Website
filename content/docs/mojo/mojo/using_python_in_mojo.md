---
title: "Interoperability with Python"
description: "Mojo Lang description"
icon: "code"
draft: false
---

### Importing Python Modules in Mojo

- Mojo's ability to import and use Python modules is a significant advantage, especially for leveraging existing Python code. It utilizes the CPython interpreter, allowing seamless integration with Python modules.

### Example: Using NumPy in Mojo

```mojo
from python import Python
let np = Python.import_module("numpy")
ar = np.arange(15).reshape(3, 5)
print(ar.shape)
```

- Note: NumPy must be installed in your Python environment.

### Limitations

- While Mojo can import Python modules, it is not yet a feature-complete superset of Python. Therefore, not all Python code can be directly copied and run in Mojo.

## Running Python Code Examples in Mojo

### 1. Basic Calculator Example

```python
def add(x, y):
    return x + y

def subtract(x, y):
    return x - y

def multiply(x, y):
    return x * y

def divide(x, y):
    return x / y

print("Select operation.")
print("1. Add")
print("2. Subtract")
print("3. Multiply")
print("4. Divide")

while True:
    choice = input("Enter choice (1/2/3/4): ")

    if choice in ('1', '2', '3', '4'):
        num1 = float(input("Enter first number: "))
        num2 = float(input("Enter second number: "))

        if choice == '1':
            print(num1, "+", num2, "=", add(num1, num2))
        elif choice == '2':
            print(num1, "-", num2, "=", subtract(num1, num2))
        elif choice == '3':
            print(num1, "*", num2, "=", multiply(num1, num2))
        elif choice == '4':
            print(num1, "/", num2, "=", divide(num1, num2))
        break
    else:
        print("Invalid Input")
```

### 2. Using Tabulate Library

```mojo
// Ensure Python tabulate is installed
from python import Python
let tabulate = Python.import_module("tabulate")

let text_data = """
Name Age Occupation
Alice 25 Engineer
Bob 30 Developer
Charlie 40 Manager
"""

let rows = [row.strip().split() for row in text_data.strip().split("\n")]
let table = tabulate.tabulate(rows, headers="firstrow")
print(table)
```

### 3. Text Extraction from Images with Tesseract OCR

```mojo
// Ensure Python pytesseract and Pillow are installed
import pytesseract
from PIL import Image

// Configure the Tesseract command if not in PATH
pytesseract.pytesseract.tesseract_cmd = r'path_to_tesseract.exe'

def read_image_text(image_path):
    image = Image.open(image_path)
    text = pytesseract.image_to_string(image)
    return text

// Usage Example
let image_path = "path_to_image.png"
let text = read_image_text(image_path)
print(text)
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
