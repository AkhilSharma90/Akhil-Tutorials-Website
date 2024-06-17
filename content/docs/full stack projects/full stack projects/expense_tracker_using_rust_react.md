---
title: "Building an Expense Tracker with Rust and React"
description: "How to install and use libraries"
icon: "code"
draft: false
---

In this tutorial, we'll walk through creating a simple expense tracker application using Rust for the backend and React for the frontend. Our Rust backend will handle expense data and provide an API for the React frontend to interact with.

## Prerequisites

Before we start, make sure you have the following installed:

- [Rust](https://www.rust-lang.org/tools/install)
- [Node.js](https://nodejs.org/en/)
- [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html)
- [Create React App](https://reactjs.org/docs/create-a-new-react-app.html#create-react-app)

## Backend: Rust with Actix-web

First, let's set up our Rust backend using Actix-web.

### Step 1: Create a new Rust project

Open your terminal and create a new Rust project:

```sh
cargo new expense_tracker_backend
cd expense_tracker_backend
```

### Step 2: Add dependencies

Open `Cargo.toml` and add the following dependencies:

```toml
[dependencies]
actix-web = "4.0"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
uuid = "1.0"
```

### Step 3: Create the backend logic

In `src/main.rs`, add the following code:

```rust
use actix_web::{web, App, HttpResponse, HttpServer, Responder};
use serde::{Deserialize, Serialize};
use std::sync::Mutex;
use uuid::Uuid;

#[derive(Serialize, Deserialize)]
struct Expense {
    id: String,
    description: String,
    amount: f64,
}

struct AppState {
    expenses: Mutex<Vec<Expense>>,
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let app_state = web::Data::new(AppState {
        expenses: Mutex::new(vec![]),
    });

    HttpServer::new(move || {
        App::new()
            .app_data(app_state.clone())
            .route("/expenses", web::get().to(get_expenses))
            .route("/expenses", web::post().to(add_expense))
    })
    .bind("127.0.0.1:8080")?
    .run()
    .await
}

async fn get_expenses(data: web::Data<AppState>) -> impl Responder {
    let expenses = data.expenses.lock().unwrap();
    HttpResponse::Ok().json(&*expenses)
}

#[derive(Deserialize)]
struct CreateExpense {
    description: String,
    amount: f64,
}

async fn add_expense(
    data: web::Data<AppState>,
    expense: web::Json<CreateExpense>,
) -> impl Responder {
    let mut expenses = data.expenses.lock().unwrap();
    let new_expense = Expense {
        id: Uuid::new_v4().to_string(),
        description: expense.description.clone(),
        amount: expense.amount,
    };
    expenses.push(new_expense);
    HttpResponse::Created().finish()
}
```

### Step 4: Run the backend

Start the backend server by running:

```sh
cargo run
```

Your backend server should now be running on `http://127.0.0.1:8080`.

## Frontend: React

Now, let's set up the React frontend.

### Step 1: Create a new React project

In a separate terminal window, create a new React project:

```sh
npx create-react-app expense-tracker-frontend
cd expense-tracker-frontend
```

### Step 2: Install Axios

We'll use Axios to make HTTP requests to our backend. Install it using:

```sh
npm install axios
```

### Step 3: Create the frontend logic

Replace the content of `src/App.js` with the following code:

```jsx
import React, { useState, useEffect } from 'react';
import axios from 'axios';
import './App.css';

function App() {
    const [expenses, setExpenses] = useState([]);
    const [description, setDescription] = useState('');
    const [amount, setAmount] = useState('');

    useEffect(() => {
        fetchExpenses();
    }, []);

    const fetchExpenses = async () => {
        const response = await axios.get('http://127.0.0.1:8080/expenses');
        setExpenses(response.data);
    };

    const addExpense = async () => {
        await axios.post('http://127.0.0.1:8080/expenses', {
            description,
            amount: parseFloat(amount),
        });
        setDescription('');
        setAmount('');
        fetchExpenses();
    };

    return (
        <div className="App">
            <h1>Expense Tracker</h1>
            <div>
                <input
                    type="text"
                    placeholder="Description"
                    value={description}
                    onChange={(e) => setDescription(e.target.value)}
                />
                <input
                    type="number"
                    placeholder="Amount"
                    value={amount}
                    onChange={(e) => setAmount(e.target.value)}
                />
                <button onClick={addExpense}>Add Expense</button>
            </div>
            <ul>
                {expenses.map((expense) => (
                    <li key={expense.id}>
                        {expense.description}: ${expense.amount.toFixed(2)}
                    </li>
                ))}
            </ul>
        </div>
    );
}

export default App;
```

### Step 4: Run the frontend

Start the React development server by running:

```sh
npm start
```

Your React application should now be running on `http://localhost:3000` and will display the expense tracker interface.

## Conclusion

In this tutorial, we created a simple expense tracker application with a Rust backend using Actix-web and a React frontend. This setup can be expanded with additional features like authentication, data persistence, and more complex interactions to create a full-fledged expense tracking system. Happy coding!
