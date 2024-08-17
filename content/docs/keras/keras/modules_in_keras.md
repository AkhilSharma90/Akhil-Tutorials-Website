---
title: "Modules in keras"
description: "Learn more about modules in keras."
icon: "K"
draft: false
---

Keras modules contains pre-defined classes, functions and variables which are useful for deep learning algorithm. Let us learn the modules provided by Keras in this chapter.

## Available modules

Let us first see the list of modules available in the Keras.

- **Initializers** − Provides a list of initializers function. We can learn it in details in Keras layer chapter. during model creation phase of machine learning.

- **Regularizers** − Provides a list of regularizers function. We can learn it in details in Keras Layers chapter.

- **Constraints** − Provides a list of constraints function. We can learn it in details in Keras Layers chapter.

- **Activations** − Provides a list of activator function. We can learn it in details in Keras Layers chapter.

- **Losses** − Provides a list of loss function. We can learn it in details in Model Training chapter.

- **Metrics** − Provides a list of metrics function. We can learn it in details in Model Training chapter.

- **Optimizers** − Provides a list of optimizer function. We can learn it in details in Model Training chapter.

- **Callback** − Provides a list of callback function. We can use it during the training process to print the intermediate data as well as to stop the training itself (EarlyStopping method) based on some condition.

- **Text processing** − Provides functions to convert text into NumPy array suitable for machine learning. We can use it in data preparation phase of machine learning.

- **Image processing** − Provides functions to convert images into NumPy array suitable for machine learning. We can use it in data preparation phase of machine learning.

- **Sequence processing** − Provides functions to generate time based data from the given input data. We can use it in data preparation phase of machine learning.

- **Backend** − Provides function of the backend library like TensorFlow and Theano.

- **Utilities** − Provides lot of utility function useful in deep learning.

Let us see backend module and utils model in this section.

## backend module

`backend module` is used for keras backend operations. By default, keras runs on top of TensorFlow backend. If you want, you can switch to other backends like Theano or CNTK. Defualt backend configuration is defined inside your root directory under .keras/keras.json file.

Keras backend module can be imported using below code

```python
>>> from keras import backend as k
```

If we are using default backend TensorFlow, then the below function returns TensorFlow based information as specified below −

```python
>>> k.backend()
'tensorflow'
>>> k.epsilon()
1e-07
>>> k.image_data_format()
'channels_last'
>>> k.floatx()
'float32'
```

Let us understand some of the significant backend functions used for data analysis in brief −

### get_uid()

It is the identifier for the default graph. It is defined below −

```python
>>> k.get_uid(prefix='')
1
>>> k.get_uid(prefix='') 2
```

### reset_uids

It is used resets the uid value.

```python
>>> k.reset_uids()
```

Now, again execute the get_uid(). This will be reset and change again to 1.

```python
>>> k.get_uid(prefix='')
1
```

### placeholder

It is used instantiates a placeholder tensor. Simple placeholder to hold 3-D shape is shown below −

```python
>>> data = k.placeholder(shape = (1,3,3))
>>> data
<tf.Tensor 'Placeholder_9:0' shape = (1, 3, 3) dtype = float32>

If you use int_shape(), it will show the shape.

>>> k.int_shape(data) (1, 3, 3)
```

### dot

It is used to multiply two tensors. Consider a and b are two tensors and c will be the outcome of multiply of ab. Assume a shape is (4,2) and b shape is (2,3). It is defined below,

```python
>>> a = k.placeholder(shape = (4,2))
>>> b = k.placeholder(shape = (2,3))
>>> c = k.dot(a,b)
>>> c
<tf.Tensor 'MatMul_3:0' shape = (4, 3) dtype = float32>
>>>
```

### ones

It is used to initialize all as one value.

```python
>>> res = k.ones(shape = (2,2))

#print the value

>>> k.eval(res)
array([[1., 1.], [1., 1.]], dtype = float32)
```

### batch_dot

It is used to perform the product of two data in batches. Input dimension must be 2 or higher. It is shown below −

```python
>>> a_batch = k.ones(shape = (2,3))
>>> b_batch = k.ones(shape = (3,2))
>>> c_batch = k.batch_dot(a_batch,b_batch)
>>> c_batch
<tf.Tensor 'ExpandDims:0' shape = (2, 1) dtype = float32>
```

### variable

It is used to initializes a variable. Let us perform simple transpose operation in this variable.

```python
>>> data = k.variable([[10,20,30,40],[50,60,70,80]])
#variable initialized here
>>> result = k.transpose(data)
>>> print(result)
Tensor("transpose_6:0", shape = (4, 2), dtype = float32)
>>> print(k.eval(result))
   [[10. 50.]
   [20. 60.]
   [30. 70.]
   [40. 80.]]
```

If you want to access from numpy −

```python
>>> data = np.array([[10,20,30,40],[50,60,70,80]])

>>> print(np.transpose(data))
   [[10 50]
   [20 60]
   [30 70]
   [40 80]]

>>> res = k.variable(value = data)
>>> print(res)
<tf.Variable 'Variable_7:0' shape = (2, 4) dtype = float32_ref>
```

### is_sparse(tensor)

It is used to check whether the tensor is sparse or not.

```python
>>> a = k.placeholder((2, 2), sparse=True)

>>> print(a) SparseTensor(indices =
   Tensor("Placeholder_8:0",
   shape = (?, 2), dtype = int64),
values = Tensor("Placeholder_7:0", shape = (?,),
dtype = float32), dense_shape = Tensor("Const:0", shape = (2,), dtype = int64))

>>> print(k.is_sparse(a)) True
```

### to_dense()

It is used to converts sparse into dense.

```python
>>> b = k.to_dense(a)
>>> print(b) Tensor("SparseToDense:0", shape = (2, 2), dtype = float32)
>>> print(k.is_sparse(b)) False
```

### random_uniform_variable

It is used to initialize using uniform distribution concept.

```python
k.random_uniform_variable(shape, mean, scale)
```

Here,

- shape − denotes the rows and columns in the format of tuples.
- mean − mean of uniform distribution.
- scale − standard deviation of uniform distribution.

Let us have a look at the below example usage −

```python
>>> a = k.random_uniform_variable(shape = (2, 3), low=0, high = 1)
>>> b = k. random_uniform_variable(shape = (3,2), low = 0, high = 1)
>>> c = k.dot(a, b)
>>> k.int_shape(c)
(2, 2)
```

## utils module

`utils` provides useful utilities function for deep learning. Some of the methods provided by the utils module is as follows −

### HDF5Matrix

It is used to represent the input data in HDF5 format.

```python
from keras.utils import HDF5Matrix data = HDF5Matrix('data.hdf5', 'data')
```

### to_categorical

It is used to convert class vector into binary class matrix.

```python
>>> from keras.utils import to_categorical
>>> labels = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
>>> to_categorical(labels)
array([[1., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
   [0., 1., 0., 0., 0., 0., 0., 0., 0., 0.],
   [0., 0., 1., 0., 0., 0., 0., 0., 0., 0.],
   [0., 0., 0., 1., 0., 0., 0., 0., 0., 0.],
   [0., 0., 0., 0., 1., 0., 0., 0., 0., 0.],
   [0., 0., 0., 0., 0., 1., 0., 0., 0., 0.],
   [0., 0., 0., 0., 0., 0., 1., 0., 0., 0.],
   [0., 0., 0., 0., 0., 0., 0., 1., 0., 0.],
   [0., 0., 0., 0., 0., 0., 0., 0., 1., 0.],
   [0., 0., 0., 0., 0., 0., 0., 0., 0., 1.]], dtype = float32)
>>> from keras.utils import normalize
>>> normalize([1, 2, 3, 4, 5])
array([[0.13483997, 0.26967994, 0.40451992, 0.53935989, 0.67419986]])
```

### print_summary

It is used to print the summary of the model.

```python
from keras.utils import print_summary print_summary(model)
```

### plot_model

It is used to create the model representation in dot format and save it to file.

```python
from keras.utils import plot_model
plot_model(model,to_file = 'image.png')
```

This plot_model will generate an image to understand the performance of model.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
