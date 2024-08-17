---
title: "Deploying PyTorch in Python via a REST API with Flask"
description: "A comprehensive introduction to PyTorch for deep learning."
icon: "code"
draft: false
---

In this tutorial, we will deploy a PyTorch model using Flask and expose a REST API for model inference. In particular, we will deploy a pretrained DenseNet 121 model which detects the image.

## API Definition

We will first define our API endpoints, the request and response types. Our API endpoint will be at /predict which takes HTTP POST requests with a file parameter which contains the image. The response will be of JSON response containing the prediction:

```sh
{"class_id": "n02124075", "class_name": "Egyptian_cat"}
```

## Dependencies

Install the required dependencies by running the following command:

```sh
pip install Flask==2.0.1 torchvision==0.10.0
```

## Simple Web Server

Following is a simple web server, taken from Flask’s documentation

```python
from flask import Flask
app = Flask(__name__)


@app.route('/')
def hello():
    return 'Hello World!'

```

We will also change the response type, so that it returns a JSON response containing ImageNet class id and name. The updated app.py file will be now:

```python
from flask import Flask, jsonify
app = Flask(__name__)

@app.route('/predict', methods=['POST'])
def predict():
    return jsonify({'class_id': 'IMAGE_NET_XXX', 'class_name': 'Cat'})

```

## Inference

In the next sections we will focus on writing the inference code. This will involve two parts, one where we prepare the image so that it can be fed to DenseNet and next, we will write the code to get the actual prediction from the model.

### Preparing the image

DenseNet model requires the image to be of 3 channel RGB image of size 224 x 224. We will also normalize the image tensor with the required mean and standard deviation values. You can read more about it here.

We will use transforms from torchvision library and build a transform pipeline, which transforms our images as required. You can read more about transforms here.

```python
import io

import torchvision.transforms as transforms
from PIL import Image

def transform_image(image_bytes):
    my_transforms = transforms.Compose([transforms.Resize(255),
                                        transforms.CenterCrop(224),
                                        transforms.ToTensor(),
                                        transforms.Normalize(
                                            [0.485, 0.456, 0.406],
                                            [0.229, 0.224, 0.225])])
    image = Image.open(io.BytesIO(image_bytes))
    return my_transforms(image).unsqueeze(0)
```

The above method takes image data in bytes, applies the series of transforms and returns a tensor. To test the above method, read an image file in bytes mode (first replacing ../\_static/img/sample_file.jpeg with the actual path to the file on your computer) and see if you get a tensor back:

```python
with open("../_static/img/sample_file.jpeg", 'rb') as f:
    image_bytes = f.read()
    tensor = transform_image(image_bytes=image_bytes)
    print(tensor)
```

## Prediction

Now will use a pretrained DenseNet 121 model to predict the image class. We will use one from torchvision library, load the model and get an inference. While we’ll be using a pretrained model in this example, you can use this same approach for your own models. See more about loading your models in this tutorial.

```python
from torchvision import models

# Make sure to set `weights` as `'IMAGENET1K_V1'` to use the pretrained weights:
model = models.densenet121(weights='IMAGENET1K_V1')
# Since we are using our model only for inference, switch to `eval` mode:
model.eval()


def get_prediction(image_bytes):
    tensor = transform_image(image_bytes=image_bytes)
    outputs = model.forward(tensor)
    _, y_hat = outputs.max(1)
    return y_hat
```

The tensor y_hat will contain the index of the predicted class id. However, we need a human readable class name. For that we need a class id to name mapping. Download this file as imagenet_class_index.json and remember where you saved it (or, if you are following the exact steps in this tutorial, save it in tutorials/\_static). This file contains the mapping of ImageNet class id to ImageNet class name. We will load this JSON file and get the class name of the predicted index.

```python
import json

imagenet_class_index = json.load(open('../_static/imagenet_class_index.json'))

def get_prediction(image_bytes):
    tensor = transform_image(image_bytes=image_bytes)
    outputs = model.forward(tensor)
    _, y_hat = outputs.max(1)
    predicted_idx = str(y_hat.item())
    return imagenet_class_index[predicted_idx]
```

Before using imagenet_class_index dictionary, first we will convert tensor value to a string value, since the keys in the imagenet_class_index dictionary are strings. We will test our above method:

```python
with open("../_static/img/sample_file.jpeg", 'rb') as f:
    image_bytes = f.read()
    print(get_prediction(image_bytes=image_bytes))
```

You should get a response like this:

```sh
['n02124075', 'Egyptian_cat']
```

The first item in array is ImageNet class id and second item is the human readable name.

## Integrating the model in our API Server

In this final part we will add our model to our Flask API server. Since our API server is supposed to take an image file, we will update our predict method to read files from the requests:

```python
from flask import request

@app.route('/predict', methods=['POST'])
def predict():
    if request.method == 'POST':
        # we will get the file from the request
        file = request.files['file']
        # convert that to bytes
        img_bytes = file.read()
        class_id, class_name = get_prediction(image_bytes=img_bytes)
        return jsonify({'class_id': class_id, 'class_name': class_name})
import io
import json

from torchvision import models
import torchvision.transforms as transforms
from PIL import Image
from flask import Flask, jsonify, request


app = Flask(__name__)
imagenet_class_index = json.load(open('<PATH/TO/.json/FILE>/imagenet_class_index.json'))
model = models.densenet121(weights='IMAGENET1K_V1')
model.eval()


def transform_image(image_bytes):
    my_transforms = transforms.Compose([transforms.Resize(255),
                                        transforms.CenterCrop(224),
                                        transforms.ToTensor(),
                                        transforms.Normalize(
                                            [0.485, 0.456, 0.406],
                                            [0.229, 0.224, 0.225])])
    image = Image.open(io.BytesIO(image_bytes))
    return my_transforms(image).unsqueeze(0)


def get_prediction(image_bytes):
    tensor = transform_image(image_bytes=image_bytes)
    outputs = model.forward(tensor)
    _, y_hat = outputs.max(1)
    predicted_idx = str(y_hat.item())
    return imagenet_class_index[predicted_idx]


@app.route('/predict', methods=['POST'])
def predict():
    if request.method == 'POST':
        file = request.files['file']
        img_bytes = file.read()
        class_id, class_name = get_prediction(image_bytes=img_bytes)
        return jsonify({'class_id': class_id, 'class_name': class_name})


if __name__ == '__main__':
    app.run()
```

```sh
FLASK_ENV=development FLASK_APP=app.py flask run
```

library to send a POST request to our app:

```python
import requests

resp = requests.post("http://localhost:5000/predict",
                     files={"file": open('<PATH/TO/.jpg/FILE>/cat.jpg','rb')})
```

Printing resp.json() will now show the following:

```sh
{"class_id": "n02124075", "class_name": "Egyptian_cat"}
```

The server we wrote is quite trivial and may not do everything you need for your production application. So, here are some things you can do to make it better:

- The endpoint /predict assumes that always there will be a image file in the request. This may not hold true for all requests. Our user may send image with a different parameter or send no images at all.
- The user may send non-image type files too. Since we are not handling errors, this will break our server. Adding an explicit error handing path that will throw an exception would allow us to better handle the bad inputs
- Even though the model can recognize a large number of classes of images, it may not be able to recognize all images. Enhance the implementation to handle cases when the model does not recognize anything in the image.
- We run the Flask server in the development mode, which is not suitable for deploying in production. You can check out this tutorial for deploying a Flask server in production.
- You can also add a UI by creating a page with a form which takes the image and displays the prediction. Check out the demo of a similar project and its source code.
- In this tutorial, we only showed how to build a service that could return predictions for a single image at a time. We could modify our service to be able to return predictions for multiple images at once. In addition, the service-streamer library automatically queues requests to your service and samples them into mini-batches that can be fed into your model. You can check out this tutorial.
- Finally, we encourage you to check out our other tutorials on deploying PyTorch models linked-to at the top of the page.
-

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
