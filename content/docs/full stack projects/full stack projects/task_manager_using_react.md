---
title: "Task Manager using Django and React"
description: "How to install and use libraries"
icon: "code"
draft: false
---

In this tutorial, we will learn the process of communicating between the Django Backend and React js frontend using the Django REST Framework. For the sake of a better understanding of the concept, we will be building a Simple Task Manager and go through the primary concepts for this type of integration between React js and Django.

### Project overview

This task manager application is a sort of to-do list. Here we will have three buttons as “Completed”, “Incomplete” and a button to add the task named “Add task”

In order to add a task, you click on the Add task button which will open up a window within the app to add the task as shown below. Here we can add the “Title” for the task and give it a description inside the “Description” section. Finally, you can check or uncheck depending upon the status of the task(ie, Completed or Incomplete)

After you “Save” the task, you can navigate between the Completed and Incomplete tabs to keep track of the tasks.

All of the operations performed above are managed by the Django REST Framework.

### Project Setup

You will need

- python 3, Node js, and a text editor.

Create a directory named “Django-react-app” using the below command(the command may change slightly depending upon your OS), moved into the directory that we just created using the below and now create a virtual environment using the below command:

```bash
mkdir django-react
cd django-react
python -m venv dar
```

We have named our virtual environment “dar”, short for Django and react. This is necessary as we don’t have to install packages and dependencies globally. It is also a good programming practice.

Now activate the virtual environment that we just created using the command below"

```bash
dar\Scripts\activate.bat
```

**Installation**
Now install Django inside the virtual machine using the below command. You will get a similar message as your installation gets completed and now let’s create our project named “backend” for our Django backend. To do so use the below command:

```bash
pip install django
django-admin startproject backend
cd backend
```

Now we will start our app and call it “todo” using the below command:

```bash
python manage.py startapp todo
```

The app gets created using the above command.

Now use the below command to migrate the project:

```bash
python manage.py migrate
```

Running the server:

```sh
python manage.py runserver
```

Now we need to take some configuration steps inside the settings.py file. In the INSTALLED_APPS section add the name of the app that we created (ie, todo) as shown below:

```python
# Application definition

INSTALLED_APPS = [
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'todo',
]

```

### Creating a Model

Next, we will need to create a Model. The Model will determine how the to-do items are stored in the database. We will have three properties in the model:

Title: This will be the title of the task with a maximum length of 150 characters.
Description: This will be the description of the task with a maximum length of 500 characters.
Completed: This will be a boolean value, that will be used to determine the current status of the task. By default, it will be set to false.
So go ahead and open the models.py file and the following code:

```python
class Todo(models.Model):
    title=models.CharField(max_length=150)
    description=models.CharField(max_length=500)
    completed=models.BooleanField(default=False)
```

We will also create a string representation of the title inside the Todo class as follows:

```python
def __str__(self):

  #it will return the title
  return self.title
```

At this point, our models.py file will look like this:

```python
from django.db import models

class Todo(models.Model):
    title=models.CharField(max_length=150)
    description=models.CharField(max_length=500)
    completed=models.BooleanField(default=False)

    # string representation of the class
    def __str__(self):

        #it will return the title
        return self.title
```

Now let’s go ahead and make migrations. Note that every time you make changes to the models.py file, we will need to make migrations. Use the below command to do so:

`python manage.py makemigrations`

Now let’s apply all migrations using the below command:

```bash
python manage.py migrate
```

Now we can test to see that the CRUD operations work on the todo model file using the Admin site (or, the interface). For this, we will need to register the models in the admin.py file.

Step 15: Open up the admin.py file and add up the following code in it:

```python
from django.contrib import admin

# import the model Todo
from .models import Todo

# create a class for the admin-model integration
class TodoAdmin(admin.ModelAdmin):

    # add the fields of the model here
    list_display = ("title","description","completed")

# we will need to register the
# model class and the Admin model class
# using the register() method
# of admin.site class
admin.site.register(Todo,TodoAdmin)
```

Now let’s create a superuser using the below command:

`python manage.py createsuperuser`

Here we will be using the following credentials:

Username: Geeks
Email address: geeks@geeksforgeeks.org
Password:12345
Note: You can set up your credentials as per your need. The above credentials need not be the same.

Now let’s run the server and check everything is going as intended so far using the below command:

```bash
python manage.py runserver
```

### Creating the API:

To create the API we will need to install the Django REST Framework for Serializers. We also need Django-cors-headers for whitelisting port 3000, which is the default port for React.

Now follow the below steps to create the Django REST framework:

Step 1: To install the Django REST framework use the below command in the backend directory:

`pip install djangorestframework`

Step 2: Now install the Django-cors-headers using the below command:

`pip install django-cors-headers`

Step 3: Now open up the setting.py file and add both the dependencies that we just installed to INSTALLED_APPS as shown below:

```python
INSTALLED_APPS = [
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'todo',
    'corsheaders',
    'rest_framework',
]
```

Step 4: Also in the settings.py file we need to whitelist the localhost port 3000. If we don’t do that there will be a block between the localhost:8000 and localhost:3000. Add the following code to achieve the same:

```python
# White listing the localhost:3000 port
# for React
CORS_ORIGIN_WHITELIST = (
    'http://localhost:3000',
)
```

Step 5: In the MIDDLEWARE section we need to add the cors-headers settings as shown below:

```python
MIDDLEWARE = [
    'django.middleware.security.SecurityMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
    'corsheaders.middleware.CorsMiddleware'
]
```

At this point, our settings.py would look like below:

```python
from pathlib import Path

# Build paths inside the project like this: BASE_DIR / 'subdir'.
BASE_DIR = Path(__file__).resolve().parent.parent


# Quick-start development settings - unsuitable for production
# See https://docs.djangoproject.com/en/3.2/howto/deployment/checklist/

# SECURITY WARNING: keep the secret key used in production secret!
SECRET_KEY = 'django-insecure-_c3!4)8+yce2l-ju@gz@b6(e0$00y@xhx7+lxk1p==k+pyqko3'

# SECURITY WARNING: don't run with debug turned on in production!
DEBUG = True

ALLOWED_HOSTS = []


# Application definition

INSTALLED_APPS = [
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'todo',
    'corsheaders',
    'rest_framework',
]

MIDDLEWARE = [
    'django.middleware.security.SecurityMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
    'corsheaders.middleware.CorsMiddleware'
]

ROOT_URLCONF = 'backend.urls'

TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [],
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
                'django.template.context_processors.debug',
                'django.template.context_processors.request',
                'django.contrib.auth.context_processors.auth',
                'django.contrib.messages.context_processors.messages',
            ],
        },
    },
]

WSGI_APPLICATION = 'backend.wsgi.application'


# Database
# https://docs.djangoproject.com/en/3.2/ref/settings/#databases

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.sqlite3',
        'NAME': BASE_DIR / 'db.sqlite3',
    }
}


# Password validation
# https://docs.djangoproject.com/en/3.2/ref/settings/#auth-password-validators

AUTH_PASSWORD_VALIDATORS = [
    {
        'NAME': 'django.contrib.auth.password_validation.UserAttributeSimilarityValidator',
    },
    {
        'NAME': 'django.contrib.auth.password_validation.MinimumLengthValidator',
    },
    {
        'NAME': 'django.contrib.auth.password_validation.CommonPasswordValidator',
    },
    {
        'NAME': 'django.contrib.auth.password_validation.NumericPasswordValidator',
    },
]

# White listing the localhost:3000 port
CORS_ORIGIN_WHITELIST = (
    'http://localhost:3000'
)

# Internationalization
# https://docs.djangoproject.com/en/3.2/topics/i18n/

LANGUAGE_CODE = 'en-us'

TIME_ZONE = 'UTC'

USE_I18N = True

USE_L10N = True

USE_TZ = True


# Static files (CSS, JavaScript, Images)
# https://docs.djangoproject.com/en/3.2/howto/static-files/

STATIC_URL = '/static/'

# Default primary key field type
# https://docs.djangoproject.com/en/3.2/ref/settings/#default-auto-field

DEFAULT_AUTO_FIELD = 'django.db.models.BigAutoField'

```

Now we need to create the Serializers for the Todo data Model. The Serializers are responsible for converting model instances to JSON. This will help the frontend to work with the received data easily. JSON is the standard for data interchange on the web.

Step 6: Now create a file inside the todo folder and name it serializers.py. Inside the folder add the following code:

```python
# import serializers from the REST framework
from rest_framework import serializers

# import the todo data model
from .models import Todo

# create a serializer class
class TodoSerializer(serializers.ModelSerializer):

    # create a meta class
    class Meta:
        model = Todo
        fields = ('id', 'title','description','completed')
```

Step 7: Now it’s time to create the Views. So open up the views.py file. Now add the following code to the file:

```python
from django.shortcuts import render

# import view sets from the REST framework
from rest_framework import viewsets

# import the TodoSerializer from the serializer file
from .serializers import TodoSerializer

# import the Todo model from the models file
from .models import Todo

# create a class for the Todo model viewsets
class TodoView(viewsets.ModelViewSet):

    # create a serializer class and
    # assign it to the TodoSerializer class
    serializer_class = TodoSerializer

    # define a variable and populate it
    # with the Todo list objects
    queryset = Todo.objects.all()
```

Step 8: Now open up the urls.py file and add the following code to it.

```python
from django.contrib import admin

# add include to the path
from django.urls import path, include

# import views from todo
from todo import views

# import routers from the REST framework
# it is necessary for routing
from rest_framework import routers

# create a router object
router = routers.DefaultRouter()

# register the router
router.register(r'tasks',views.TodoView, 'task')

urlpatterns = [
    path('admin/', admin.site.urls),

    # add another path to the url patterns
    # when you visit the localhost:8000/api
    # you should be routed to the django Rest framework
    path('api/', include(router.urls))


]
```

This is the final step for creating the REST API and we can now perform all CRUD operations. Routers allow us to make queries. For example, if we go the “tasks”, this will return the list of all the tasks. Also, you can have a single ‘task’ with an id to return a single task, where id is the primary key.

### Creating UI

Now let’s build the frontend for our Todo app. For this follow the below steps:

Step 1: Navigate to the main project directory(ie, Django-react-app) and activate the virtual environment using the below command:

```bash
dar\Scripts\activate.bat
```

Step 2: Now use the below command to create a boilerplate of React js app:

```bash
npx create-react-app frontend
```

Step 3: So, use the below command to install reactstrap and bootstrap in the project:

`npm install reactstrap bootstrap`

Step 4: First move into the Frontend folder and use the below command to run the React server to make sure everything if working till this point:

`npm start`
If everything is fine, you’ll get the following page on the localhost:3000

Step 5: Now open up the App.js file in the frontend folder. And clear the boilerplate code and change it to the below code:

```jsx
Javascript;
import "./App.css";

function App() {
  return (
    <div className="App">
      <h2>Welcome to Geeksforgeeks!</h2>
    </div>
  );
}

export default App;
```

At this point the frontend will look like below:

As you can see in the above image. Any changes made to the App.js file are reflected directly to the UI.

Step 6: Now the code to the App.js file. Comments are added to the code for better understanding.

```
// import Component from the react module
import React, { Component } from "react";
import Modal from "./components/Modal";
import axios from 'axios';

// create a class that extends the component
class App extends Component {

  // add a constructor to take props
  constructor(props) {
    super(props);

    // add the props here
    this.state = {

      // the viewCompleted prop represents the status
      // of the task. Set it to false by default
      viewCompleted: false,
      activeItem: {
        title: "",
        description: "",
        completed: false
      },

      // this list stores all the completed tasks
      taskList: []
    };
  }

  // Add componentDidMount()
  componentDidMount() {
    this.refreshList();
  }


  refreshList = () => {
    axios   //Axios to send and receive HTTP requests
      .get("http://localhost:8000/api/tasks/")
      .then(res => this.setState({ taskList: res.data }))
      .catch(err => console.log(err));
  };

  // this arrow function takes status as a parameter
  // and changes the status of viewCompleted to true
  // if the status is true, else changes it to false
  displayCompleted = status => {
    if (status) {
      return this.setState({ viewCompleted: true });
    }
    return this.setState({ viewCompleted: false });
  };

  // this array function renders two spans that help control
  // the set of items to be displayed(ie, completed or incomplete)
  renderTabList = () => {
    return (
      <div className="my-5 tab-list">
        <span
          onClick={() => this.displayCompleted(true)}
          className={this.state.viewCompleted ? "active" : ""}
        >
          completed
            </span>
        <span
          onClick={() => this.displayCompleted(false)}
          className={this.state.viewCompleted ? "" : "active"}
        >
          Incompleted
            </span>
      </div>
    );
  };
  // Main variable to render items on the screen
  renderItems = () => {
    const { viewCompleted } = this.state;
    const newItems = this.state.taskList.filter(
      (item) => item.completed === viewCompleted
    );
    return newItems.map((item) => (
      <li
        key={item.id}
        className="list-group-item d-flex justify-content-between align-items-center"
      >
        <span
          className={`todo-title mr-2 ${
            this.state.viewCompleted ? "completed-todo" : ""
          }`}
          title={item.description}
        >
          {item.title}
        </span>
        <span>
          <button
            onClick={() => this.editItem(item)}
            className="btn btn-secondary mr-2"
          >
            Edit
          </button>
          <button
            onClick={() => this.handleDelete(item)}
            className="btn btn-danger"
          >
            Delete
          </button>
        </span>
      </li>
    ));
  };

  toggle = () => {
    //add this after modal creation
    this.setState({ modal: !this.state.modal });
  };


  // Submit an item
  handleSubmit = (item) => {
    this.toggle();
     alert("save" + JSON.stringify(item));
    if (item.id) {
      // if old post to edit and submit
      axios
        .put(`http://localhost:8000/api/tasks/${item.id}/`, item)
        .then((res) => this.refreshList());
      return;
    }
    // if new post to submit
    axios
      .post("http://localhost:8000/api/tasks/", item)
      .then((res) => this.refreshList());
  };

  // Delete item
  handleDelete = (item) => {
      alert("delete" + JSON.stringify(item));
    axios
      .delete(`http://localhost:8000/api/tasks/${item.id}/`)
      .then((res) => this.refreshList());
  };

  // Create item
  createItem = () => {
    const item = { title: "", description: "", completed: false };
    this.setState({ activeItem: item, modal: !this.state.modal });
  };

  //Edit item
  editItem = (item) => {
    this.setState({ activeItem: item, modal: !this.state.modal });
  };

  // Start by visual effects to viewer
  render() {
    return (
      <main className="content">
        <h1 className="text-success text-uppercase text-center my-4">
          GFG Task Manager
        </h1>
        <div className="row ">
          <div className="col-md-6 col-sm-10 mx-auto p-0">
            <div className="card p-3">
              <div className="">
                <button onClick={this.createItem} className="btn btn-info">
                  Add task
                </button>
              </div>
              {this.renderTabList()}
              <ul className="list-group list-group-flush">
                {this.renderItems()}
              </ul>
            </div>
          </div>
        </div>
        {this.state.modal ? (
          <Modal
            activeItem={this.state.activeItem}
            toggle={this.toggle}
            onSave={this.handleSubmit}
          />
        ) : null}
      </main>
    );
  }
}
export default App;
```

Step 7: Now open up the Index.css file, clear the CSS inside it and add the following CSS in the file:

```css
CSS .todo-title {
  cursor: pointer;
}
.completed-todo {
  text-decoration: line-through;
}
.tab-list > span {
  padding: 5px 8px;
  border: 1px solid rgb(7, 167, 68);
  border-radius: 10px;
  margin-right: 5px;
  cursor: pointer;
}
.tab-list > span.active {
  background-color: rgb(6, 139, 12);
  color: #fff;
}
```

Step 8: Now create a new folder named “Components” in the src directory and add a file Modal.js to it. Then add the following code to it.

```
Javascript
import React, { Component } from "react";

// importing all of these classes from reactstrap module
import {
  Button,
  Modal,
  ModalHeader,
  ModalBody,
  ModalFooter,
  Form,
  FormGroup,
  Input,
  Label
} from "reactstrap";

// build a class base component
class CustomModal extends Component {
  constructor(props) {
    super(props);
    this.state = {
      activeItem: this.props.activeItem
    };
  }
  // changes handler to check if a checkbox is checked or not
  handleChange = e => {
    let { name, value } = e.target;
    if (e.target.type === "checkbox") {
      value = e.target.checked;
    }
    const activeItem = { ...this.state.activeItem, [name]: value };
    this.setState({ activeItem });
  };

  // rendering modal in the custommodal class received toggle and on save as props,
  render() {
    const { toggle, onSave } = this.props;
    return (
      <Modal isOpen={true} toggle={toggle}>
        <ModalHeader toggle={toggle}> Task Item </ModalHeader>
        <ModalBody>

          <Form>

            {/* 3 formgroups
            1 title label */}
            <FormGroup>
              <Label for="title">Title</Label>
              <Input
                type="text"
                name="title"
                value={this.state.activeItem.title}
                onChange={this.handleChange}
                placeholder="Enter Task Title"
              />
            </FormGroup>

            {/* 2 description label */}
            <FormGroup>
              <Label for="description">Description</Label>
              <Input
                type="text"
                name="description"
                value={this.state.activeItem.description}
                onChange={this.handleChange}
                placeholder="Enter Task Description"
              />
            </FormGroup>

            {/* 3 completed label */}
            <FormGroup check>
              <Label for="completed">
                <Input
                  type="checkbox"
                  name="completed"
                  checked={this.state.activeItem.completed}
                  onChange={this.handleChange}
                />
                Completed
              </Label>
            </FormGroup>
          </Form>
        </ModalBody>
        {/* create a modal footer */}
        <ModalFooter>
          <Button color="success" onClick={() => onSave(this.state.activeItem)}>
            Save
          </Button>
        </ModalFooter>
      </Modal>
    );
  }
}
export default CustomModal
```

Step 10: Make the changes to the index.js file as follows:

```jsx
import React from "react";
import ReactDOM from "react-dom";
import "./index.css";
import App from "./App";

// importing css stylesheet to use the bootstrap class
// add this line only in this file
import "bootstrap/dist/css/bootstrap.min.css";

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById("root")
);
```

### API Connection:

For us to make requests to the API endpoints on the backend server of Django, we will need to install Axios. Use the following command inside the frontend folder to install Axios:

`npm install axios`

Congratulation!!. At this point, you have successfully build a Fullstack Django-React app and Used the Django REST framework to establish communication between the frontend and backend.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
