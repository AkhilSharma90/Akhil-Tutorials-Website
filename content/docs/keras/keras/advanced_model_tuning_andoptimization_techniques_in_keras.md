---
title: "7. Advanced Model Tuning and Optimization Techniques in Keras"
description: ""
icon: "K"
draft: false
---
This tutorial covers advanced techniques for tuning and optimizing models in Keras, including hyperparameter tuning, regularization methods, and learning rate schedulers.

<!-- ## Table of Contents
1. [Hyperparameter Tuning](#hyperparameter-tuning)
    - [Grid Search](#grid-search)
    - [Random Search](#random-search)
    - [Bayesian Optimization](#bayesian-optimization)
2. [Regularization Techniques](#regularization-techniques)
    - [L1 and L2 Regularization](#l1-and-l2-regularization)
    - [Dropout](#dropout)
    - [Batch Normalization](#batch-normalization)
3. [Learning Rate Schedulers](#learning-rate-schedulers)
    - [Step Decay](#step-decay)
    - [Exponential Decay](#exponential-decay)
    - [LearningRateScheduler Callback](#learningratescheduler-callback) -->

## Hyperparameter Tuning

Hyperparameter tuning is crucial for improving the performance of machine learning models. Keras provides several ways to perform hyperparameter tuning.

### Grid Search

Grid search exhaustively searches through a specified subset of hyperparameters. It is straightforward but can be computationally expensive.

```python
from sklearn.model_selection import GridSearchCV
from keras.wrappers.scikit_learn import KerasClassifier

def create_model(optimizer='adam'):
    model = Sequential()
    model.add(Dense(12, input_dim=8, activation='relu'))
    model.add(Dense(1, activation='sigmoid'))
    model.compile(loss='binary_crossentropy', optimizer=optimizer, metrics=['accuracy'])
    return model

model = KerasClassifier(build_fn=create_model, verbose=0)
optimizers = ['rmsprop', 'adam']
epochs = [50, 100]
batches = [5, 10]
param_grid = dict(optimizer=optimizers, epochs=epochs, batch_size=batches)
grid = GridSearchCV(estimator=model, param_grid=param_grid, n_jobs=-1, cv=3)
grid_result = grid.fit(X, Y)

print(f"Best: {grid_result.best_score_} using {grid_result.best_params_}")
```

### Random Search

Random search samples a wide range of hyperparameters randomly. It is often more efficient than grid search.

```python
from sklearn.model_selection import RandomizedSearchCV

param_dist = dict(optimizer=optimizers, epochs=epochs, batch_size=batches)
random = RandomizedSearchCV(estimator=model, param_distributions=param_dist, n_iter=10, n_jobs=-1, cv=3)
random_result = random.fit(X, Y)

print(f"Best: {random_result.best_score_} using {random_result.best_params_}")
```

### Bayesian Optimization

Bayesian optimization models the function that maps hyperparameters to the objective value and uses this model to select the next set of hyperparameters to evaluate.

```python
from keras_tuner import BayesianOptimization

def build_model(hp):
    model = Sequential()
    model.add(Dense(units=hp.Int('units', min_value=32, max_value=512, step=32), activation='relu'))
    model.add(Dense(1, activation='sigmoid'))
    model.compile(optimizer=hp.Choice('optimizer', ['adam', 'rmsprop']), loss='binary_crossentropy', metrics=['accuracy'])
    return model

tuner = BayesianOptimization(build_model, objective='val_accuracy', max_trials=10, executions_per_trial=3)
tuner.search(X_train, y_train, epochs=50, validation_data=(X_val, y_val))

best_hps = tuner.get_best_hyperparameters(num_trials=1)[0]
print(f"Best hyperparameters: {best_hps.values}")
```

## Regularization Techniques

Regularization techniques help prevent overfitting by adding a penalty to the model's complexity.

### L1 and L2 Regularization

L1 regularization adds the absolute value of the coefficients, while L2 adds the squared value.

```python
from keras.regularizers import l1, l2

model.add(Dense(64, input_dim=64, activation='relu', kernel_regularizer=l2(0.01)))
model.add(Dense(64, activation='relu', kernel_regularizer=l1(0.01)))
```

### Dropout

Dropout randomly sets a fraction of input units to 0 at each update during training time, which helps prevent overfitting.

```python
from keras.layers import Dropout

model.add(Dropout(0.5))
```

### Batch Normalization

Batch normalization normalizes the inputs of each layer so that they have a mean of 0 and a standard deviation of 1.

```python
from keras.layers import BatchNormalization

model.add(BatchNormalization())
```

## Learning Rate Schedulers

Learning rate schedulers adjust the learning rate during training, which can help improve performance and convergence.

### Step Decay

Step decay reduces the learning rate by a factor every few epochs.

```python
def step_decay(epoch):
    initial_lr = 0.1
    drop = 0.5
    epochs_drop = 10.0
    lr = initial_lr * (drop ** np.floor((1+epoch)/epochs_drop))
    return lr

from keras.callbacks import LearningRateScheduler

lr_scheduler = LearningRateScheduler(step_decay)
model.fit(X_train, y_train, epochs=100, callbacks=[lr_scheduler])
```

### Exponential Decay

Exponential decay reduces the learning rate exponentially over epochs.

```python
def exp_decay(epoch):
    initial_lr = 0.1
    k = 0.1
    lr = initial_lr * np.exp(-k*epoch)
    return lr

lr_scheduler = LearningRateScheduler(exp_decay)
model.fit(X_train, y_train, epochs=100, callbacks=[lr_scheduler])
```

### LearningRateScheduler Callback

Keras provides a `LearningRateScheduler` callback to implement custom learning rate schedules.

```python
def custom_lr_schedule(epoch, lr):
    if epoch < 10:
        return lr
    else:
        return lr * tf.math.exp(-0.1)

lr_scheduler = LearningRateScheduler(custom_lr_schedule)
model.fit(X_train, y_train, epochs=100, callbacks=[lr_scheduler])
```

By applying these advanced tuning and optimization techniques, you can significantly improve the performance and efficiency of your Keras models.
