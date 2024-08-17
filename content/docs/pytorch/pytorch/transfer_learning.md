---
title: "Transferr Learning in Pytorch"
description: "A comprehensive introduction to PyTorch for deep learning."
icon: "code"
draft: false
---

## Transfer Learning with PyTorch: Enhancing Your Classification Model

### What is Transfer Learning?

Transfer learning allows us to leverage pre-trained models, using the patterns and knowledge they have acquired from large datasets to solve new, but related, problems. This technique is highly valuable in scenarios where acquiring a massive dataset or training a model from scratch is impractical.

### Why Use Transfer Learning?

1. **Leverage Existing Models**: Use neural network architectures that are already proven to work on similar problems.
2. **Efficiency**: Achieve better results with less data and computational resources by building on models that have already learned useful patterns.

### Setting Up Transfer Learning with PyTorch

Let's dive into the practical steps to set up a transfer learning workflow using PyTorch, focusing on an image classification task with FoodVision Mini (pizza, steak, sushi).

#### 0. Setup

Ensure you have the necessary libraries and modules. We'll also download required scripts and data.

```python
# Imports and setup
import torch
import torchvision
from torch import nn
from torchvision import transforms

try:
    from torchinfo import summary
except ImportError:
    !pip install torchinfo
    from torchinfo import summary

# Download going_modular scripts
try:
    from going_modular.going_modular import data_setup, engine
except ImportError:
    !git clone https://github.com/mrdbourke/pytorch-deep-learning
    !mv pytorch-deep-learning/going_modular .
    !rm -rf pytorch-deep-learning
    from going_modular.going_modular import data_setup, engine

# Setup device
device = "cuda" if torch.cuda.is_available() else "cpu"
```

#### 1. Get Data

Download and prepare the dataset.

```python
import os
import zipfile
from pathlib import Path
import requests

# Setup data path
data_path = Path("data/")
image_path = data_path / "pizza_steak_sushi"

if not image_path.is_dir():
    image_path.mkdir(parents=True, exist_ok=True)
    with open(data_path / "pizza_steak_sushi.zip", "wb") as f:
        request = requests.get("https://github.com/mrdbourke/pytorch-deep-learning/raw/main/data/pizza_steak_sushi.zip")
        f.write(request.content)
    with zipfile.ZipFile(data_path / "pizza_steak_sushi.zip", "r") as zip_ref:
        zip_ref.extractall(image_path)
    os.remove(data_path / "pizza_steak_sushi.zip")

train_dir = image_path / "train"
test_dir = image_path / "test"
```

#### 2. Create Datasets and DataLoaders

We need to prepare our data loaders with appropriate transformations.

```python
# Define manual transforms
manual_transforms = transforms.Compose([
    transforms.Resize((224, 224)),
    transforms.ToTensor(),
    transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225])
])

# Create DataLoaders
train_dataloader, test_dataloader, class_names = data_setup.create_dataloaders(
    train_dir=train_dir,
    test_dir=test_dir,
    transform=manual_transforms,
    batch_size=32
)
```

#### 3. Load a Pretrained Model

Choose a pretrained model and customize it for our dataset.

```python
# Load pretrained EfficientNet_B0
weights = torchvision.models.EfficientNet_B0_Weights.DEFAULT
model = torchvision.models.efficientnet_b0(weights=weights).to(device)

# Print model summary
summary(model, input_size=(32, 3, 224, 224), col_names=["input_size", "output_size", "num_params", "trainable"], col_width=20, row_settings=["var_names"])
```

#### 4. Customize the Model

Freeze the base layers and adjust the output layer for our specific task.

```python
# Freeze base layers
for param in model.features.parameters():
    param.requires_grad = False

# Modify the classifier head
model.classifier[1] = nn.Linear(in_features=model.classifier[1].in_features, out_features=len(class_names)).to(device)

# Verify model changes
summary(model, input_size=(32, 3, 224, 224), col_names=["input_size", "output_size", "num_params", "trainable"], col_width=20, row_settings=["var_names"])
```

#### 5. Train the Model

Use a suitable optimizer and loss function, then train the model.

```python
# Set up loss function and optimizer
loss_fn = nn.CrossEntropyLoss()
optimizer = torch.optim.Adam(model.classifier.parameters(), lr=1e-3)

# Train the model
engine.train(model=model, train_dataloader=train_dataloader, test_dataloader=test_dataloader, optimizer=optimizer, loss_fn=loss_fn, epochs=5, device=device)
```

### Conclusion

By following these steps, you can effectively utilize transfer learning to boost your model's performance with minimal data and effort. This technique not only saves time and resources but also leverages the power of pre-trained models to achieve superior results on custom tasks.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
