---
title: "Build a Text Summarizer using MERN"
description: "How to install and use libraries"
icon: "code"
draft: false
---

The MERN stack, comprising MongoDB, Express, React, and Node.js, is a powerful combination for building full-stack web applications. In this article, we’ll walk you through the process of creating a Summarizer Website using the MERN stack.

### 1. Pre-requisites

Before we begin, ensure you have the following knowledge and tools:

- Basic knowledge of JavaScript.
- Familiarity with the MERN Stack basics.
- Basic knowledge of API integration.
- Understanding of how to set up and use MongoDB with Mongoose.

### 2. Approach to Create Summarizer Website

1. **Generate an OpenAI API Key** to use the OpenAI API.
2. **Set up the MERN project** in an IDE of your choice (like Visual Studio Code).
3. **Integrate the OpenAI API Key** into the application server-side codebase.
4. **Launch the application**.

### 3. Steps to Generate OpenAI API Key

#### Step 1: Log in to OpenAI

- Open the OpenAI platform
- Create a new account or log in with your existing account.
- Navigate to the OpenAI dashboard.

#### Step 2: Generate API Key

- Hover on the left sidebar and click on ‘API Keys’.
- Click on ‘+ Create new secret key’.

**Note:** Save this API Key in a safe place; it will be required later.

### 4. Steps to Create the Project

#### Step 1: Set Up the React App

Create your React App and install the required dependencies.

```bash
npm create vite@latest
# you will be prompted to enter project name, choose summarizer-app, then choose React framework
cd summarizer-app
npm install
```

We will also need `axios` so run 
```sh
npm install axios
```

Create a `.env` file inside the root directory of the React app:

```plaintext
REACT_APP_BACKEND_URL=http://localhost:5000
```

Update the dependencies in `package.json`:

```json
"dependencies": {
    "@testing-library/jest-dom": "^5.17.0",
    "@testing-library/react": "^13.4.0",
    "@testing-library/user-event": "^13.5.0",
    "axios": "^1.6.3",
    "react": "^18.2.0",
    "react-dom": "^18.2.0",
    "react-scripts": "5.0.1",
    "web-vitals": "^2.1.4"
}
```

Add the following code to `src/App.js`:

```jsx
// src/App.js
import axios from 'axios';
import React, { useState } from 'react';
 
const App = () => {
  const [inputText, setInputText] = useState('');
  const [summary, setSummary] = useState('');
 
  const summarizeText = async () => {
    try {
      const response = await axios.post(
        `${process.env.REACT_APP_BACKEND_URL}/api/summarize`,
        { text: inputText }
      );
      setSummary(response.data.summary);
    } catch (error) {
      console.error('Error calling backend API:', error);
    }
  };
 
  return (
    <div>
      <h1>Text Summarizer</h1>
      <textarea
        rows="10"
        cols="50"
        value={inputText}
        onChange={(e) => setInputText(e.target.value)}
      ></textarea>
      <br />
      <button onClick={summarizeText}>Summarize</button>
      <h2>Summary:</h2>
      <p>{summary}</p>
    </div>
  );
};
 
export default App;
```

#### Step 2: Set Up the Express Server

Create the server directory and install the required dependencies:

```bash
mkdir server
cd server
npm init -y
npm install express axios body-parser dotenv mongoose cors
```

Create a `.env` file in the server folder:

```plaintext
OPENAI_API_KEY=your_openai_api_key
MONGODB_URI=your_mongodb_connection_uri
```

Replace `your_openai_api_key` and `your_mongodb_connection_uri` with your actual OpenAI API Key and MongoDB URI.

Update the dependencies in `package.json`:

```json
"dependencies": {
    "axios": "^1.6.5",
    "body-parser": "^1.20.2",
    "dotenv": "^16.3.1",
    "express": "^4.18.2",
    "mongoose": "^6.1.12",
    "cors": "^2.8.5"
}
```

Create the `server.js` file in the server directory:

```js
// server/server.js

require("dotenv").config();
const OpenAI = require("openai");
const express = require("express");
const axios = require("axios");
const bodyParser = require("body-parser");
const mongoose = require("mongoose");
const cors = require("cors");

const app = express();
const PORT = process.env.PORT || 5000;

app.use(bodyParser.json());
app.use(cors());

const OPENAI_API_KEY = process.env.OPENAI_API_KEY;

// Connect to MongoDB
mongoose.connect(process.env.MONGODB_URI, {
    useNewUrlParser: true,
    useUnifiedTopology: true,
});

// Define a schema for summaries
const summarySchema = new mongoose.Schema({
    text: String,
    summarizedText: String,
});

// Define a model for the schema
const Summary = mongoose.model("Summary", summarySchema);

app.post("/api/summarize", async (req, res) => {
    const { text } = req.body;

    const openai = new OpenAI({
        apiKey: OPENAI_API_KEY,
    });

    try {
        const response = await openai.chat.completions.create({
            model: "gpt-3.5-turbo",
            messages: [
                {
                    role: "system",
                    content: `Summarize content you are provided with for a second-grade student.`,
                },
                {
                    role: "user",
                    content: text,
                },
            ],
            temperature: 0.7,
            max_tokens: 64,
            top_p: 1,
        });

        const summarizedText = String(response.choices[0].message.content);

        // Save the summary to MongoDB
        const newSummary = new Summary({ text, summarizedText });
        await newSummary.save();

        res.json({ summary: summarizedText });
    } catch (error) {
        console.error("Error calling OpenAI API:", error);
        res.status(500).json({ error: "Internal Server Error" });
    }
});

app.listen(PORT, () => {
    console.log(`Server is running on port ${PORT}`);
});
```

#### Step 3: Start the Development Servers

In one terminal window, navigate to the server directory and start the Express server:

```bash
cd server
node server.js
```

In another terminal window, navigate to the root directory of your React app and start the React development server:

```bash
cd summarizer-app
npm run dev
```

### 5. Testing the Application

Open your browser and navigate to `http://localhost:5173`. Enter some text into the textarea and click the "Summarize" button to see the summary generated by the OpenAI API.

### 6. Conclusion

In this tutorial, we built a Summarizer Website using the MERN stack. We covered how to set up the React frontend, the Express backend, and how to integrate the OpenAI API to generate text summaries. This project demonstrates the power of the MERN stack in building full-stack applications and how you can leverage external APIs to add powerful features to your apps.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
