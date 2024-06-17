---
title: "Simple Password Validator using React + Go"
description: "How to install and use libraries"
icon: "code"
draft: false
---

## Building a Password Validation Tool with Go and React

In this blog post, we'll walk you through creating a simple yet powerful password validation tool using Go for the backend and React for the frontend. This tutorial will cover everything from setting up the server, implementing password validation logic, and building a responsive client interface to test the password strength.

### Table of Contents

1. Introduction
2. Setting Up the Server
   - Installing Dependencies
   - Writing the `main.go` File
3. Implementing Password Validation Logic
4. Creating the React Client
   - Setting Up the React Project
   - Writing the `App.js` File
5. Testing the Application
6. Conclusion

---

### 1. Introduction

Password security is a crucial aspect of any web application. Ensuring that users create strong passwords can help protect their accounts from being compromised. In this tutorial, we will create a tool that validates password strength based on specific criteria such as length, and the inclusion of lowercase letters, uppercase letters, and numbers.

### 2. Setting Up the Server

First, we need to set up the server using Go and the Gin framework.

#### Installing Dependencies

To get started, make sure you have Go installed on your machine. Next, install the necessary Go packages:

```bash
go get -u github.com/gin-gonic/gin
go get -u github.com/gin-contrib/cors
```

#### Writing the `main.go` File

Create a file named `main.go` and add the following code:

```go
//Server
// main.go
package main

import (
	"net/http"
	"regexp"

	"github.com/gin-contrib/cors"
	"github.com/gin-gonic/gin"
)

type PasswordRequest struct {
	Password string `json:"password"`
}

type PasswordResponse struct {
	IsStrong bool   `json:"isStrong"`
	Message  string `json:"message"`
}

func validatePassword(password string) (bool, string) {
	var hasMinLength = regexp.MustCompile(`.{8,}`)
	var hasLower = regexp.MustCompile(`[a-z]`)
	var hasUpper = regexp.MustCompile(`[A-Z]`)
	var hasNumber = regexp.MustCompile(`[0-9]`)

	if !hasMinLength.MatchString(password) {
		return false, "Password must be at least 8 characters long"
	}
	if !hasLower.MatchString(password) {
		return false, "Password must have at least one lowercase letter"
	}
	if !hasUpper.MatchString(password) {
		return false, "Password must have at least one uppercase letter"
	}
	if !hasNumber.MatchString(password) {
		return false, "Password must have at least one number"
	}
	return true, "Strong password"
}

func passwordHandler(c *gin.Context) {
	var req PasswordRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}

	isStrong, message := validatePassword(req.Password)
	res := PasswordResponse{IsStrong: isStrong, Message: message}

	c.JSON(http.StatusOK, res)
}

func main() {
	r := gin.Default()

	// Configure CORS middleware
	r.Use(cors.New(cors.Config{
		AllowOrigins: []string{"http://localhost:5173"},
	}))

	r.POST("/validate-password", passwordHandler)
	r.Run(":8080")
}
```

In this code, we set up a basic server using Gin and add an endpoint to validate the password. The `validatePassword` function checks if the password meets the required criteria.

### 3. Implementing Password Validation Logic

The `validatePassword` function uses regular expressions to ensure that the password:

- Is at least 8 characters long
- Contains at least one lowercase letter
- Contains at least one uppercase letter
- Contains at least one number

If any of these criteria are not met, it returns a corresponding error message.

### 4. Creating the React Client

Next, we need to create a client using React to interact with our server.

#### Setting Up the React Project

First, set up a new React project using Create React App:

```bash
npm create vite@latest
# it will prompt you to create the project name, and choose the framework. Choose React.
cd password-validator
npm install
```

#### Writing the `App.js` File

Replace the contents of `src/App.js` with the following code:

```jsx
import React, { useState } from "react";

const App = () => {
  const [errorMessage, setErrorMessage] = useState("");

  const validate = async (value) => {
    try {
      const response = await fetch("http://localhost:8080/validate-password", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ password: value }),
      });

      const data = await response.json();
      if (data.isStrong) {
        setErrorMessage("Is Strong Password");
      } else {
        setErrorMessage(data.message);
      }
    } catch (error) {
      setErrorMessage("Error validating password");
    }
  };

  return (
    <div style={{ marginLeft: "200px" }}>
      <h2>Checking Password Strength in ReactJS</h2>
      <span>Enter Password: </span>
      <input type="text" onChange={(e) => validate(e.target.value)}></input>
      <br />
      {errorMessage === "" ? null : (
        <span style={{ fontWeight: "bold", color: "red" }}>{errorMessage}</span>
      )}
    </div>
  );
};

export default App;
```

In this code, we create a simple React component that allows the user to enter a password. The `validate` function sends the password to the server and displays the validation result.

### 5. Testing the Application

To test the application, start the Go server and the React application:

1. Start the Go server:

```bash
go run main.go
```

2. Start the React application:

```bash
npm run dev
```

Open your browser and navigate to `http://localhost:3000`. Enter a password and see the validation result.

![image](https://i.imgur.com/uc2iLCW.png)

### 6. Conclusion

In this tutorial, we built a simple password validation tool using Go and React. This tool can be extended with additional validation rules or integrated into a larger application to enhance security. Understanding how to validate passwords and provide feedback to users is an essential skill for any developer working on user authentication and security.
