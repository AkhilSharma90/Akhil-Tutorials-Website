---
title: "Deep Dive into PyTorch: Autograd and Neural Networks"
description: "Exploring Autograd and Neural Networks in PyTorch."
icon: "code"
draft: false
---

## Autograd: Automatic Differentiation in PyTorch

PyTorch's autograd is a powerful tool for automatically computing gradients of tensor operations. Understanding autograd is essential for training neural networks efficiently.

### How Autograd Works

Autograd works by automatically tracking operations performed on tensors and then using this information to compute gradients during backpropagation. When you perform operations on tensors with `requires_grad=True`, PyTorch builds a computational graph to track the operations. During the backward pass, gradients are computed using the chain rule of calculus and propagated back through the graph.

Let's illustrate this with an example:

```python
import torch

# Define tensors with requires_grad=True
x = torch.tensor(2.0, requires_grad=True)
y = torch.tensor(3.0, requires_grad=True)

# Perform operations
z = x * y + 2

# Compute gradients
z.backward()

print(x.grad)  # Output: tensor(3.)
print(y.grad)  # Output: tensor(2.)
```

In this example, `x.grad` will be 3.0 because the gradient of `z` with respect to `x` is 3. Similarly, `y.grad` will be 2.0 because the gradient of `z` with respect to `y` is 2.

### Neural Networks with PyTorch

PyTorch provides a flexible and efficient framework for building neural networks. The `torch.nn` module allows you to define custom neural network architectures with ease.

#### Example: Creating a Simple Neural Network

```python
import torch
import torch.nn as nn

class SimpleNN(nn.Module):
    def __init__(self):
        super(SimpleNN, self).__init__()
        self.fc1 = nn.Linear(10, 50)
        self.relu = nn.ReLU()
        self.fc2 = nn.Linear(50, 1)

    def forward(self, x):
        x = self.fc1(x)
        x = self.relu(x)
        x = self.fc2(x)
        return x

# Instantiate the network
model = SimpleNN()
```

### Training a Neural Network

Once you have defined a neural network, you can train it using gradient descent optimization algorithms such as stochastic gradient descent (SGD) or Adam.

#### Example: Training a Simple Neural Network

```python
# Define loss function and optimizer
criterion = nn.MSELoss()
optimizer = torch.optim.SGD(model.parameters(), lr=0.01)

# Training loop
for epoch in range(num_epochs):
    # Forward pass
    outputs = model(inputs)
    loss = criterion(outputs, targets)

    # Backward pass and optimization
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

    # Print progress
    if (epoch+1) % 10 == 0:
        print(f'Epoch [{epoch+1}/{num_epochs}], Loss: {loss.item():.4f}')
```

### Conclusion

Autograd and neural networks are fundamental components of PyTorch that enable you to build and train complex deep learning models. By leveraging autograd for automatic differentiation and defining custom neural network architectures, you can tackle a wide range of machine learning tasks effectively.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
