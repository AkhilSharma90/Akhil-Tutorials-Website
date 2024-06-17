---
title: "Creating a Simple Chat Application using Go and React"
description: "How to install and use libraries"
icon: "code"
draft: false
---

### Creating a Simple Chat Application with Go (Backend)

In this tutorial, we'll walk through the process of building a simple chat application backend using Go. The backend will leverage WebSockets to allow real-time communication between clients. Here's a detailed explanation of the provided code, covering the main components and concepts used.

#### Table of Contents

1. WebSocket Client
    - Client Struct
    - Message Struct
    - Read Method
2. WebSocket Pool
    - Pool Struct
    - NewPool Function
    - Start Method
3. WebSocket Upgrade
    - Upgrade Function
4. Main Application
    - serveWS Function
    - setupRoutes Function
    - main Function

### 1. WebSocket Client

#### Client Struct

```go
package websocket

import (
	"sync"
	"github.com/gorilla/websocket"
)

type Client struct {
	ID   string
	Conn *websocket.Conn
	Pool *Pool
	mu   sync.Mutex
}
```

The `Client` struct represents a connected WebSocket client. It includes:

- `ID`: A unique identifier for the client.
- `Conn`: The WebSocket connection.
- `Pool`: A reference to the pool that manages this client.
- `mu`: A mutex for handling concurrent access.

#### Message Struct

```go
type Message struct {
	Type int    `json:"type"`
	Body string `json:"body"`
}
```

The `Message` struct represents a message exchanged between clients. It includes:

- `Type`: The type of message (text, binary, etc.).
- `Body`: The message content.

#### Read Method

```go
func (c *Client) Read() {
	defer func() {
		c.Pool.Unregister <- c
		c.Conn.Close()
	}()

	for {
		messageType, p, err := c.Conn.ReadMessage()
		if err != nil {
			log.Println(err)
			return
		}

		message := Message{Type: messageType, Body: string(p)}
		c.Pool.Broadcast <- message
		fmt.Printf("Message Received: %+v\n", message)
	}
}
```

The `Read` method continuously listens for incoming messages from the client's WebSocket connection. When a message is received, it is broadcast to all other clients via the pool.

- `defer`: Ensures that the client is unregistered and the connection is closed when the method exits.
- `Conn.ReadMessage()`: Reads a message from the WebSocket connection.
- `c.Pool.Broadcast`: Sends the message to all connected clients.

### 2. WebSocket Pool

#### Pool Struct

```go
package websocket

type Pool struct {
	Register   chan *Client
	Unregister chan *Client
	Clients    map[*Client]bool
	Broadcast  chan Message
}
```

The `Pool` struct manages all active WebSocket clients. It includes:

- `Register`: A channel for registering new clients.
- `Unregister`: A channel for unregistering clients.
- `Clients`: A map of active clients.
- `Broadcast`: A channel for broadcasting messages to all clients.

#### NewPool Function

```go
func NewPool() *Pool {
	return &Pool{
		Register:   make(chan *Client),
		Unregister: make(chan *Client),
		Clients:    make(map[*Client]bool),
		Broadcast:  make(chan Message),
	}
}
```

The `NewPool` function creates and returns a new instance of `Pool`.

#### Start Method

```go
func (pool *Pool) Start() {
	for {
		select {
		case client := <-pool.Register:
			pool.Clients[client] = true
			fmt.Println("Size of Connection Pool: ", len(pool.Clients))
			for client := range pool.Clients {
				client.Conn.WriteJSON(Message{Type: 1, Body: "New User Joined..."})
			}
		case client := <-pool.Unregister:
			delete(pool.Clients, client)
			fmt.Println("Size of Connection Pool: ", len(pool.Clients))
			for client := range pool.Clients {
				client.Conn.WriteJSON(Message{Type: 1, Body: "User Disconnected..."})
			}
		case message := <-pool.Broadcast:
			fmt.Println("Sending message to all clients in Pool")
			for client := range pool.Clients {
				if err := client.Conn.WriteJSON(message); err != nil {
					fmt.Println(err)
					return
				}
			}
		}
	}
}
```

The `Start` method continuously listens for registration, unregistration, and broadcast events.

- `Register`: Adds a new client to the pool and notifies all clients.
- `Unregister`: Removes a client from the pool and notifies all clients.
- `Broadcast`: Sends a message to all clients in the pool.

### 3. WebSocket Upgrade

#### Upgrade Function

```go
package websocket

import (
	"log"
	"net/http"

	"github.com/gorilla/websocket"
)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func Upgrade(w http.ResponseWriter, r *http.Request) (*websocket.Conn, error) {
	upgrader.CheckOrigin = func(r *http.Request) bool { return true }
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Println(err)
		return nil, err
	}

	return conn, nil
}
```

The `Upgrade` function upgrades an HTTP connection to a WebSocket connection using the Gorilla WebSocket package.

- `upgrader`: Configures WebSocket connection parameters.
- `Upgrade`: Upgrades the HTTP connection and returns the WebSocket connection.

### 4. Main Application

#### serveWS Function

```go
package main

import (
	"fmt"
	"net/http"

	"github.com/akhil/golang-chat/pkg/websocket"
)

func serveWS(pool *websocket.Pool, w http.ResponseWriter, r *http.Request) {
	fmt.Println("websocket endpoint reached")

	conn, err := websocket.Upgrade(w, r)

	if err != nil {
		fmt.Fprintf(w, "%+v\n", err)
	}
	client := &websocket.Client{
		Conn: conn,
		Pool: pool,
	}
	pool.Register <- client
	client.Read()
}
```

The `serveWS` function handles WebSocket requests. It upgrades the connection, creates a new client, and starts listening for messages from that client.

#### setupRoutes Function

```go
func setupRoutes() {
	pool := websocket.NewPool()
	go pool.Start()

	http.HandleFunc("/ws", func(w http.ResponseWriter, r *http.Request) {
		serveWS(pool, w, r)
	})
}
```

The `setupRoutes` function sets up the HTTP routes for the application. It creates a new pool, starts it, and defines the WebSocket endpoint.

#### main Function

```go
func main() {
	fmt.Println("Akhil's full stack chat project")
	setupRoutes()
	http.ListenAndServe(":9000", nil)
}
```

The `main` function starts the HTTP server on port 9000 and sets up the routes.


### Building a Simple Chat Application with Go and React (Frontend)

Continuing from where we left off with the backend setup using Go, we now focus on creating the frontend using React. This frontend will communicate with our Go WebSocket server to send and receive messages in real-time.

### Table of Contents

1. Setting Up WebSocket Connection
    - `src/api/index.js`
2. Chat History Component
    - `src/components/ChatHistory.jsx`
3. Chat Input Component
    - `src/components/ChatInput.jsx`
4. Header Component
    - `src/components/Header.jsx`
5. Message Component
    - `src/components/Message.jsx`
6. Main Application Component
    - `src/App.jsx`

---

### 1. Setting Up WebSocket Connection

#### `src/api/index.js`

```javascript
// api/index.js
var socket = new WebSocket('ws://localhost:9000/ws');

let connect = (cb) => {
  console.log("connecting")

  socket.onopen = () => {
    console.log("Successfully Connected");
  }
  
  socket.onmessage = (msg) => {
    console.log("Message from WebSocket: ", msg);
    cb(msg);
  }

  socket.onclose = (event) => {
    console.log("Socket Closed Connection: ", event)
  }

  socket.onerror = (error) => {
    console.log("Socket Error: ", error)
  }
};

let sendMsg = (msg) => {
  console.log("sending msg: ", msg);
  socket.send(msg);
};

export { connect, sendMsg };
```

This module sets up a WebSocket connection to the backend server and provides functions to connect and send messages.

- `connect(cb)`: Establishes the connection and sets up event listeners for WebSocket events (`onopen`, `onmessage`, `onclose`, `onerror`). The callback `cb` is called whenever a message is received.
- `sendMsg(msg)`: Sends a message to the WebSocket server.

### 2. Chat History Component

#### `src/components/ChatHistory.jsx`

```javascript
import React, { Component } from 'react';
import './ChatHistory.scss';
import Message from '../Message/Message';

class ChatHistory extends Component {
  render() {
    console.log(this.props.chatHistory);
    const messages = this.props.chatHistory.map(msg => <Message key={msg.timeStamp} message={msg.data} />);

    return (
      <div className='ChatHistory'>
        <h2>Chat History</h2>
        {messages}
      </div>
    );
  };
}

export default ChatHistory;
```

The `ChatHistory` component displays the chat history. It maps through the `chatHistory` prop and renders a `Message` component for each message.

- `this.props.chatHistory`: An array of messages passed down from the parent component.
- `messages`: An array of `Message` components created from `chatHistory`.

### 3. Chat Input Component

#### `src/components/ChatInput.jsx`

```javascript
import React, { Component } from 'react';
import './ChatInput.scss';

class ChatInput extends Component {
  render() {
    return (
      <div className='ChatInput'>
        <input onKeyDown={this.props.send} placeholder="Type a message... Hit Enter to Send"/>
      </div>
    );
  };
}

export default ChatInput;
```

The `ChatInput` component renders an input field where users can type their messages. When the user presses the "Enter" key, the `send` function from the parent component is called.

- `this.props.send`: A function passed down from the parent component to handle sending messages.

### 4. Header Component

#### `src/components/Header.jsx`

```javascript
import React from 'react';
import './Header.scss';

const Header = () => (
  <div className='header'>
    <h2>Go + React Socket Chat</h2>
  </div>
);

export default Header;
```

The `Header` component displays a static header for the chat application.

### 5. Message Component

#### `src/components/Message.jsx`

```javascript
import React, { Component } from 'react';
import './Message.scss';

class Message extends Component {
  constructor(props) {
    super(props);
    let temp = JSON.parse(this.props.message);
    this.state = {
      message: temp
    }
  }
  
  render() {
    return (
      <div className='Message'>
        {this.state.message.body}
      </div>
    );
  };
}

export default Message;
```

The `Message` component renders a single chat message. It parses the `message` prop (assumed to be JSON) and displays its `body` field.

- `constructor(props)`: Parses the `message` prop and sets the initial state.
- `this.state.message.body`: The body of the message to be displayed.

### 6. Main Application Component

Finally, you need a main component to bring everything together. Create `src/App.jsx`:

```javascript
import React, { Component } from 'react';
import { connect, sendMsg } from './api';
import Header from './components/Header';
import ChatHistory from './components/ChatHistory';
import ChatInput from './components/ChatInput';
import './App.scss';

class App extends Component {
  constructor(props) {
    super(props);
    this.state = {
      chatHistory: []
    };
  }

  componentDidMount() {
    connect((msg) => {
      console.log("New Message");
      this.setState(prevState => ({
        chatHistory: [...prevState.chatHistory, msg]
      }));
      console.log(this.state);
    });
  }

  send(event) {
    if (event.keyCode === 13) {
      sendMsg(event.target.value);
      event.target.value = '';
    }
  }

  render() {
    return (
      <div className="App">
        <Header />
        <ChatHistory chatHistory={this.state.chatHistory} />
        <ChatInput send={(e) => this.send(e)} />
      </div>
    );
  }
}

export default App;
```

In the `App` component:

- `state`: Holds the chat history.
- `componentDidMount()`: Establishes the WebSocket connection and updates the chat history state when a new message is received.
- `send(event)`: Sends a message when the "Enter" key is pressed in the input field and clears the input field.
- `render()`: Renders the `Header`, `ChatHistory`, and `ChatInput` components.

### Conclusion

We have successfully built a simple chat application using Go for the backend and React for the frontend. The backend handles WebSocket connections, while the frontend allows users to send and receive messages in real-time. This project demonstrates the fundamentals of real-time communication in web applications.