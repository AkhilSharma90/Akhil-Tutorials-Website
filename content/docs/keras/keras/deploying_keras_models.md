---
title: "6. Deploying Keras Models"
description: "Deploying a model is key in ensuring that other people can use it"
icon: "K"
draft: false
---

This tutorial covers the process of deploying Keras models, including exporting models, deploying on different platforms, and optimizing models for deployment.

## Exporting Models

Exporting models is the first step towards deploying them. Keras models can be saved and loaded in different formats.

### Saving Models

Keras models can be saved in two formats: HDF5 and SavedModel.

#### HDF5 Format

```python
model.save('model.h5')
```

#### SavedModel Format

```python
model.save('saved_model/')
```

### Loading Models

Models saved in HDF5 or SavedModel format can be loaded back into Keras.

#### Loading from HDF5

```python
from keras.models import load_model

model = load_model('model.h5')
```

#### Loading from SavedModel

```python
from keras.models import load_model

model = load_model('saved_model/')
```

## Model Deployment on Different Platforms

Keras models can be deployed on various platforms for different use cases.

### TensorFlow Serving

TensorFlow Serving is a flexible, high-performance serving system for machine learning models designed for production environments.

#### Exporting the Model for TensorFlow Serving

```python
import tensorflow as tf

model.save('saved_model/', save_format='tf')
```

#### Serving the Model

```bash
tensorflow_model_server --rest_api_port=8501 --model_name=my_model --model_base_path="/path/to/saved_model/"
```

### Flask

Flask is a lightweight WSGI web application framework in Python that can be used to deploy machine learning models.

#### Creating a Flask App

```python
from flask import Flask, request, jsonify
from keras.models import load_model
import numpy as np

app = Flask(__name__)
model = load_model('model.h5')

@app.route('/predict', methods=['POST'])
def predict():
    data = request.json
    prediction = model.predict(np.array(data['input']))
    return jsonify({'prediction': prediction.tolist()})

if __name__ == '__main__':
    app.run(port=5000, debug=True)
```

### FastAPI

FastAPI is a modern, fast (high-performance), web framework for building APIs with Python 3.7+ based on standard Python type hints.

#### Creating a FastAPI App

```python
from fastapi import FastAPI
from pydantic import BaseModel
from keras.models import load_model
import numpy as np

app = FastAPI()
model = load_model('model.h5')

class PredictionRequest(BaseModel):
    input: list

@app.post('/predict')
def predict(request: PredictionRequest):
    data = np.array(request.input)
    prediction = model.predict(data)
    return {'prediction': prediction.tolist()}

if __name__ == '__main__':
    import uvicorn
    uvicorn.run(app, host='0.0.0.0', port=8000)
```

### TensorFlow Lite

TensorFlow Lite is an open-source deep learning framework for on-device inference.

#### Converting a Model to TensorFlow Lite

```python
import tensorflow as tf

model = tf.keras.models.load_model('model.h5')
converter = tf.lite.TFLiteConverter.from_keras_model(model)
tflite_model = converter.convert()

with open('model.tflite', 'wb') as f:
    f.write(tflite_model)
```

#### Running Inference with TensorFlow Lite

```python
import numpy as np
import tensorflow as tf

interpreter = tf.lite.Interpreter(model_path='model.tflite')
interpreter.allocate_tensors()

input_details = interpreter.get_input_details()
output_details = interpreter.get_output_details()

input_data = np.array([[...]], dtype=np.float32)
interpreter.set_tensor(input_details[0]['index'], input_data)
interpreter.invoke()

output_data = interpreter.get_tensor(output_details[0]['index'])
print(output_data)
```

## Model Optimization for Deployment

Optimizing models for deployment can improve performance, reduce latency, and decrease resource consumption.

### Model Quantization

Quantization reduces the precision of the numbers used to represent your model's parameters, which can result in smaller model size and faster inference.

#### Post-Training Quantization

```python
converter = tf.lite.TFLiteConverter.from_keras_model(model)
converter.optimizations = [tf.lite.Optimize.DEFAULT]
tflite_model = converter.convert()
```

### Model Pruning

Pruning removes weights that contribute less to the output, which can reduce model size and improve inference speed.

#### Applying Pruning

```python
import tensorflow_model_optimization as tfmot

prune_low_magnitude = tfmot.sparsity.keras.prune_low_magnitude

pruning_params = {
    'pruning_schedule': tfmot.sparsity.keras.PolynomialDecay(
        initial_sparsity=0.0,
        final_sparsity=0.5,
        begin_step=0,
        end_step=end_step
    )
}

model_for_pruning = prune_low_magnitude(model, **pruning_params)
model_for_pruning.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])
model_for_pruning.fit(x_train, y_train, epochs=2)
```

### TensorFlow Model Optimization Toolkit

The TensorFlow Model Optimization Toolkit provides a suite of techniques for optimizing machine learning models for deployment and execution.

#### Applying Optimization

```python
import tensorflow_model_optimization as tfmot

model = tf.keras.models.load_model('model.h5')

# Pruning
prune_low_magnitude = tfmot.sparsity.keras.prune_low_magnitude
pruning_params = {'pruning_schedule': tfmot.sparsity.keras.PolynomialDecay(initial_sparsity=0.0, final_sparsity=0.5, begin_step=0, end_step=end_step)}
model = prune_low_magnitude(model, **pruning_params)

# Quantization-aware training
quantize_model = tfmot.quantization.keras.quantize_model
model = quantize_model(model)

model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])
model.fit(x_train, y_train, epochs=2)
```

By following these steps, you can effectively deploy and optimize your Keras models for various platforms and applications.
