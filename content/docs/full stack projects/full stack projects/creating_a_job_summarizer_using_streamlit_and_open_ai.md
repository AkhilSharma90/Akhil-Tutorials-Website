---
title: "Job Description Summarizer with Streamlit and OpenAI"
description: "How to install and use libraries"
icon: "code"
draft: false
---

In this tutorial, we'll build a web application that summarizes job descriptions using AI. We'll use Streamlit for the frontend and OpenAI's GPT model for generating the summaries. The application will take a job description as input and output a summarized version, highlighting the job title, responsibilities, and requirements.

### 1. Introduction

We'll create a web application that uses OpenAI's GPT model to summarize job descriptions. The summarizer will extract and categorize important information from the job description, such as the job title, responsibilities, and requirements, and provide insights on how to highlight these skills in a resume.

### 2. Setting Up the Environment

First, make sure you have Python installed on your machine. Then, install the required libraries:

```bash
pip install streamlit openai langchain environs
```

### 3. Writing the Summarizer Function

Create a directory named `app` and within it, create a file named `summarizer.py`. This file will contain the logic to summarize the job descriptions using OpenAI's GPT model.

#### `app/summarizer.py`

```python
import os
from environs import Env

from langchain import PromptTemplate
from langchain.llms import OpenAI

env = Env()
env.read_env(".env")  # read .env file, if it exists

api_key = os.getenv("OPENAI_API_KEY")

def summarize_jd(job_description):
    template = """
    You are a career services professional with over 10 years of experience in the {industry} primarily operating in the {location}. You are tasked with finding skills from a job description for the role of {role}. Here is the job description:

    {job_description}

    Please analyze this job description to identify the skills, and categorize them as follows:

    - Job Title: Assess the job title as it may contain key terms that should be mirrored in the resume.
    - Responsibilities: Focus on responsibilities and action verbs that align with the role.
    - Requirements: Look for requirements such as education, experience, and certifications.

    Advise on how these identified skills can be effectively highlighted in a resume to ensure it aligns well with the requirements of this role. Please avoid cliches and generic answers, focusing on unique and specific insights related to the provided job description.
    """

    prompt = PromptTemplate(
        input_variables=["industry", "location", "role", "job_description"],
        template=template,
    )

    # Formats the prompt with the addition of the input variables
    final_prompt = prompt.format(industry="Software Engineering", location="United States", role="Golang Engineer", job_description=job_description)

    llm = OpenAI(openai_api_key=api_key)

    summary = llm(final_prompt)

    return summary
```

In this file, we define a function `summarize_jd` that takes a job description as input and returns a summarized version. The function uses OpenAI's GPT model to generate the summary based on a custom prompt template.

### 4. Building the Streamlit App

Create a file named `main.py` in the root directory of your project. This file will contain the Streamlit code to build the web interface.

#### `main.py`

```python
import streamlit as st
from app.summarizer import summarize_jd

# Set page title
st.set_page_config(page_title="Job Description Summarizer", page_icon="📜", layout="wide")

# Set title
st.title("Job Description Summarizer", anchor=False)
st.header("Summarize Job Description with AI", anchor=False)

with st.form("my_form"):
    # Input JD (pasted by user)
    jd = st.text_area("Paste the job description here", value="")

    def submit_form():
        summary = summarize_jd(jd)
        st.write(summary)

    submit = st.form_submit_button(label="Summarize", on_click=submit_form)
```

In this file, we create a Streamlit app with a form where users can paste a job description. When the form is submitted, the `summarize_jd` function is called to generate the summary, which is then displayed on the page.

### 5. Running the Application

To run the application, navigate to the project directory in your terminal and run:

```bash
streamlit run main.py
```

This command will start a local Streamlit server and open the application in your default web browser.

### Conclusion

In this tutorial, we created a web application that summarizes job descriptions using AI. We used Streamlit for the frontend and OpenAI's GPT model for generating the summaries. This application can be extended further to include more advanced features like user authentication, saving summaries, and more. This project demonstrates the power of combining Streamlit and OpenAI to create useful AI-powered tools.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
