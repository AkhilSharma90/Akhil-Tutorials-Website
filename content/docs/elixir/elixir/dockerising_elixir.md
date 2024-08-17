---
title: "Using Docker and Elixir"
description: "Learn how to build a simple cli todo tool using elixir"
icon: "code"
draft: false
---

### Introduction

Phoenix is known for its ability to facilitate rapid development of Elixir web services. The Mix tool further enhances productivity by automating repetitive tasks such as compiling code and running tests. Containerization adds another layer of efficiency, especially when deploying applications with supporting infrastructure.

In this tutorial, we will explore how to run a Phoenix application using Docker and Docker Compose, and create a custom Mix task to integrate containerization into the Elixir workflow seamlessly.

### Goals

By the end of this tutorial, you will:

1. Build a Docker container to run a Phoenix release.
2. Set up an orchestrated multi-container environment with Docker Compose.
3. Create a custom Mix task to automate Docker tasks and simplify containerization.

### Prerequisites

To follow this tutorial, you will need:

- A basic understanding of Docker.
- Familiarity with Elixir’s Mix tool.
- Docker and Docker Compose installed on your workstation.
- Elixir and Phoenix installed on your workstation.
- A basic Phoenix application (you can create one by following the [Phoenix Guides](https://hexdocs.pm/phoenix/up_and_running.html)).

### Setting Up Your Phoenix Application

1. **Create a new Phoenix project:**

   ```sh
   mix phx.new my_app
   cd my_app
   ```

2. **Set up the database configuration:**
   Edit `config/dev.exs` and `config/prod.exs` to configure your database connection.

3. **Create the database and run migrations:**
   ```sh
   mix ecto.create
   mix ecto.migrate
   ```

### Building the Phoenix Release

1. **Compile the project:**

   ```sh
   MIX_ENV=prod mix compile
   ```

2. **Create the release:**
   ```sh
   MIX_ENV=prod mix release
   ```

### Building a Docker Container

1. **Create a Dockerfile in the root of your project:**

   ```Dockerfile
   FROM elixir:1.12-alpine
   RUN apk add --no-cache build-base npm git postgresql-client
   WORKDIR /app
   COPY . .
   RUN mix deps.get --only prod
   RUN MIX_ENV=prod mix compile
   RUN npm install --prefix assets
   RUN npm run deploy --prefix assets
   RUN MIX_ENV=prod mix phx.digest
   RUN MIX_ENV=prod mix release

   CMD ["_build/prod/rel/my_app/bin/my_app", "start"]
   ```

2. **Build the Docker image:**

   ```sh
   docker build -t my_app .
   ```

3. **Run the Docker container:**
   ```sh
   docker run -p 4000:4000 my_app
   ```

### Setting Up Docker Compose

1. **Create a `docker-compose.yml` file:**

   ```yaml
   version: "3"
   services:
     web:
       build: .
       ports:
         - "4000:4000"
       depends_on:
         - db
       environment:
         DATABASE_URL: "ecto://postgres:postgres@db/postgres"
         SECRET_KEY_BASE: "a_secret_key"

     db:
       image: postgres:13
       environment:
         POSTGRES_USER: postgres
         POSTGRES_PASSWORD: postgres
         POSTGRES_DB: postgres
       volumes:
         - db_data:/var/lib/postgresql/data

   volumes:
     db_data:
   ```

2. **Build and run the services:**
   ```sh
   docker-compose up --build
   ```

### Creating a Custom Mix Task

1. **Create a new Mix task in `lib/mix/tasks/compose.ex`:**

   ```elixir
   defmodule Mix.Tasks.Compose do
     use Mix.Task

     @shortdoc "Run Docker Compose to manage the application containers"
     def run(args) do
       case Mix.shell.cmd("docker-compose --version", quiet: true) do
         0 -> compose(args)
         _ -> Mix.shell.error("Docker Compose is not installed. Please install it from https://docs.docker.com/compose/install/")
       end
     end

     defp compose(["up"]) do
       Mix.shell.cmd("docker-compose up --build")
     end

     defp compose(["down"]) do
       Mix.shell.cmd("docker-compose down")
     end

     defp compose(["logs"]) do
       Mix.shell.cmd("docker-compose logs")
     end

     defp compose(_) do
       Mix.shell.info("Usage: mix compose [up|down|logs]")
     end
   end
   ```

2. **Use the new Mix task:**
   ```sh
   mix compose up
   mix compose logs
   mix compose down
   ```

### Conclusion

In this tutorial, we have:

- Built and run a Docker container for a Phoenix release.
- Set up a multi-container environment with Docker Compose.
- Created a custom Mix task to manage Docker tasks.

This setup simplifies development and deployment workflows, making it easier to manage your Phoenix applications in a containerized environment.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
