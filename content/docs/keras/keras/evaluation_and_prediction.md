---
title: "Model evaludation and prediction in keras(Advanced)"
description: "Learn about model evaluation and prediction in keras."
icon: "K"
draft: false
---

This chapter deals with the model evaluation and model prediction in Keras.

Let us begin by understanding the model evaluation.

## Model Evaluation
Evaluation is a process during development of the model to check whether the model is best fit for the given problem and corresponding data. Keras model provides a function, evaluate which does the evaluation of the model. It has three main arguments,

- Test data
- Test data label
- verbose - true or false

Let us evaluate the model, which we created in the previous chapter using test data.

```python
score = model.evaluate(x_test, y_test, verbose = 0) 

print('Test loss:', score[0]) 
print('Test accuracy:', score[1])
```

Executing the above code will output the below information.
```bash
0
```

The test accuracy is 98.28%. We have created a best model to identify the handwriting digits. On the positive side, we can still scope to improve our model.

## Model Prediction
Prediction is the final step and our expected outcome of the model generation. Keras provides a method, predict to get the prediction of the trained model. The signature of the predict method is as follows,

```python
predict(
   x, 
   batch_size = None, 
   verbose = 0, 
   steps = None, 
   callbacks = None, 
   max_queue_size = 10, 
   workers = 1, 
   use_multiprocessing = False
)
```

Here, all arguments are optional except the first argument, which refers the unknown input data. The shape should be maintained to get the proper prediction.

Let us do prediction for our MPL model created in previous chapter using below code −

```python
pred = model.predict(x_test) 
pred = np.argmax(pred, axis = 1)[:5] 
label = np.argmax(y_test,axis = 1)[:5] 

print(pred) 
print(label)
```

Here,

- Line 1 call the predict function using test data.
- Line 2 gets the first five prediction
- Line 3 gets the first five labels of the test data.
- Line 5 - 6 prints the prediction and actual label.

The output of the above application is as follows −

```bash
[7 2 1 0 4] 
[7 2 1 0 4]
```
The output of both array is identical and it indicate that our model predicts correctly the first five images.