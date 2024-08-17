---
title: "Training models on arbitrary data sources"
description: "All Keras models can be trained and evaluated on a wide variety of data sources, independently of the backend you're using."
icon: "K"
draft: false
---

All Keras models can be trained and evaluated on a wide variety of data sources, independently of the backend you're using. This includes:

- NumPy arrays
- Pandas dataframes
- TensorFlow tf.data.Dataset objects
- PyTorch DataLoader objects
- Keras PyDataset objects

They all work whether you're using TensorFlow, JAX, or PyTorch as your Keras backend.

Let's try it out with PyTorch `DataLoaders`:

```python
import torch

# Create a TensorDataset
train_torch_dataset = torch.utils.data.TensorDataset(
    torch.from_numpy(x_train), torch.from_numpy(y_train)
)
val_torch_dataset = torch.utils.data.TensorDataset(
    torch.from_numpy(x_test), torch.from_numpy(y_test)
)

# Create a DataLoader
train_dataloader = torch.utils.data.DataLoader(
    train_torch_dataset, batch_size=batch_size, shuffle=True
)
val_dataloader = torch.utils.data.DataLoader(
    val_torch_dataset, batch_size=batch_size, shuffle=False
)

model = MyModel(num_classes=10)
model.compile(
    loss=keras.losses.SparseCategoricalCrossentropy(),
    optimizer=keras.optimizers.Adam(learning_rate=1e-3),
    metrics=[
        keras.metrics.SparseCategoricalAccuracy(name="acc"),
    ],
)
model.fit(train_dataloader, epochs=1, validation_data=val_dataloader)
```

```bash
 469/469 ━━━━━━━━━━━━━━━━━━━━ 81s 172ms/step - acc: 0.5502 - loss: 1.2550 - val_acc: 0.9419 - val_loss: 0.1972

<keras.src.callbacks.history.History at 0x2b3385480>
```

Now let's try this out with `tf.data`:

```python
import tensorflow as tf

train_dataset = (
    tf.data.Dataset.from_tensor_slices((x_train, y_train))
    .batch(batch_size)
    .prefetch(tf.data.AUTOTUNE)
)
test_dataset = (
    tf.data.Dataset.from_tensor_slices((x_test, y_test))
    .batch(batch_size)
    .prefetch(tf.data.AUTOTUNE)
)

model = MyModel(num_classes=10)
model.compile(
    loss=keras.losses.SparseCategoricalCrossentropy(),
    optimizer=keras.optimizers.Adam(learning_rate=1e-3),
    metrics=[
        keras.metrics.SparseCategoricalAccuracy(name="acc"),
    ],
)
model.fit(train_dataset, epochs=1, validation_data=test_dataset)
```

```bash
 469/469 ━━━━━━━━━━━━━━━━━━━━ 81s 172ms/step - acc: 0.5771 - loss: 1.1948 - val_acc: 0.9229 - val_loss: 0.2502

<keras.src.callbacks.history.History at 0x2b33e7df0>
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
