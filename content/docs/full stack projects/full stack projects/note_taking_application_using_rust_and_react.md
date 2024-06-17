---
title: "Building a Note-Taking Application with Rust and React"
description: ""
icon: "code"
draft: false
---

In this tutorial, we'll build a simple note-taking application using Rust for the backend and React for the frontend. Our Rust backend will handle note data and provide a RESTful API for the React frontend to interact with.

## Prerequisites

Before we start, make sure you have the following installed:

- [Rust](https://www.rust-lang.org/tools/install)
- [Node.js](https://nodejs.org/en/)
- [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html)
- [SQLite](https://www.sqlite.org/download.html)
- [Create React App](https://reactjs.org/docs/create-a-new-react-app.html#create-react-app)

## Backend: Rust with Actix-web and SQLite

### Step 1: Create a new Rust project

Open your terminal and create a new Rust project:

```sh
cargo new note_taking_backend
cd note_taking_backend
```

### Step 2: Add dependencies

Open `Cargo.toml` and add the following dependencies:

```toml
[dependencies]
actix-web = "4.0"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
serde_derive = "1.0"
sqlx = { version = "0.5", features = ["sqlite", "runtime-actix-native-tls"] }
uuid = "1.0"
dotenv = "0.15"
```

### Step 3: Set up the SQLite database

Create a `.env` file in the root of your project with the following content:

```plaintext
DATABASE_URL=sqlite:notes.db
```

### Step 4: Create the backend logic

In `src/main.rs`, add the following code:

```rust
use actix_web::{web, App, HttpResponse, HttpServer, Responder};
use serde::{Deserialize, Serialize};
use sqlx::{sqlite::SqlitePool, Pool};
use std::sync::Mutex;
use uuid::Uuid;
use dotenv::dotenv;
use std::env;

#[derive(Serialize, Deserialize)]
struct Note {
    id: String,
    title: String,
    content: String,
}

struct AppState {
    db_pool: Pool<sqlx::Sqlite>,
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    dotenv().ok();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");

    let db_pool = SqlitePool::connect(&database_url).await.unwrap();

    sqlx::query(
        "CREATE TABLE IF NOT EXISTS notes (
            id TEXT PRIMARY KEY,
            title TEXT NOT NULL,
            content TEXT NOT NULL
        )",
    )
    .execute(&db_pool)
    .await
    .unwrap();

    let app_state = web::Data::new(AppState { db_pool });

    HttpServer::new(move || {
        App::new()
            .app_data(app_state.clone())
            .route("/notes", web::get().to(get_notes))
            .route("/notes", web::post().to(add_note))
    })
    .bind("127.0.0.1:8080")?
    .run()
    .await
}

async fn get_notes(data: web::Data<AppState>) -> impl Responder {
    let notes = sqlx::query_as!(Note, "SELECT id, title, content FROM notes")
        .fetch_all(&data.db_pool)
        .await
        .unwrap();

    HttpResponse::Ok().json(notes)
}

#[derive(Deserialize)]
struct CreateNote {
    title: String,
    content: String,
}

async fn add_note(
    data: web::Data<AppState>,
    note: web::Json<CreateNote>,
) -> impl Responder {
    let new_note = Note {
        id: Uuid::new_v4().to_string(),
        title: note.title.clone(),
        content: note.content.clone(),
    };

    sqlx::query!(
        "INSERT INTO notes (id, title, content) VALUES (?, ?, ?)",
        new_note.id,
        new_note.title,
        new_note.content
    )
    .execute(&data.db_pool)
    .await
    .unwrap();

    HttpResponse::Created().finish()
}
```

### Step 5: Run the backend

Start the backend server by running:

```sh
cargo run
```

Your backend server should now be running on `http://127.0.0.1:8080`.

## Frontend: React

### Step 1: Create a new React project

In a separate terminal window, create a new React project:

```sh
npx create-react-app note-taking-frontend
cd note-taking-frontend
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
    const [notes, setNotes] = useState([]);
    const [title, setTitle] = useState('');
    const [content, setContent] = useState('');

    useEffect(() => {
        fetchNotes();
    }, []);

    const fetchNotes = async () => {
        const response = await axios.get('http://127.0.0.1:8080/notes');
        setNotes(response.data);
    };

    const addNote = async () => {
        await axios.post('http://127.0.0.1:8080/notes', {
            title,
            content,
        });
        setTitle('');
        setContent('');
        fetchNotes();
    };

    return (
        <div className="App">
            <h1>Note Taking App</h1>
            <div>
                <input
                    type="text"
                    placeholder="Title"
                    value={title}
                    onChange={(e) => setTitle(e.target.value)}
                />
                <textarea
                    placeholder="Content"
                    value={content}
                    onChange={(e) => setContent(e.target.value)}
                ></textarea>
                <button onClick={addNote}>Add Note</button>
            </div>
            <ul>
                {notes.map((note) => (
                    <li key={note.id}>
                        <h2>{note.title}</h2>
                        <p>{note.content}</p>
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

Your React application should now be running on `http://localhost:3000` and will display the note-taking interface.

## Conclusion

In this tutorial, we created a simple note-taking application with a Rust backend using Actix-web and SQLite for data storage, and a React frontend. This setup can be expanded with additional features like authentication, search functionality, and more to create a fully-featured note-taking application. Happy coding!
