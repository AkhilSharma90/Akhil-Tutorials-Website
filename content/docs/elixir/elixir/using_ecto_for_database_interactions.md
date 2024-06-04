---
title: "Using Ecto for Database Interactions"
description: "Elixir is a process-oriented, functional programming language that runs on the Erlang virtual machine (BEAM). The language was influenced by Ruby. This inspiration can be seen and felt in Elixir’s ecosystem and tooling options. Elixir is known to be easy to learn and widely applicable within the software development industry."
icon: "code"
draft: false
---

Ecto is a powerful database library and toolkit for Elixir, providing a domain-specific language for interacting with databases. This tutorial covers the essentials of using Ecto, from setting up and configuring your database to writing queries and handling data validation with changesets.

## Introduction to Ecto

Ecto is an Elixir library for interacting with databases. It allows you to define schemas, run queries, manage database changes through migrations, and validate data. Ecto supports PostgreSQL, MySQL, SQLite, and other databases.

## Setting Up Ecto

To start using Ecto in your project, follow these steps:

1. Add Ecto and a database adapter (e.g., PostgreSQL) to your `mix.exs` dependencies:
    ```elixir
    defp deps do
      [
        {:ecto_sql, "~> 3.7"},
        {:postgrex, ">= 0.0.0"}
      ]
    end
    ```

2. Fetch the dependencies:
    ```sh
    mix deps.get
    ```

3. Generate the Ecto repository:
    ```sh
    mix ecto.gen.repo -r MyApp.Repo
    ```

4. Configure the repository in `config/config.exs`:
    ```elixir
    config :my_app, MyApp.Repo,
      username: "postgres",
      password: "postgres",
      database: "my_app_dev",
      hostname: "localhost",
      show_sensitive_data_on_connection_error: true,
      pool_size: 10

    config :my_app, ecto_repos: [MyApp.Repo]
    ```

5. Create the repository:
    ```sh
    mix ecto.create
    ```

## Defining Schemas

Schemas define the structure of your data and how it maps to database tables. Create a schema module using the `mix phx.gen.schema` task or manually.

Example of a manually created schema for a `User`:

1. Create a file `lib/my_app/user.ex`:
    ```elixir
    defmodule MyApp.User do
      use Ecto.Schema
      import Ecto.Changeset

      schema "users" do
        field :name, :string
        field :email, :string
        field :age, :integer
        timestamps()
      end

      def changeset(user, attrs) do
        user
        |> cast(attrs, [:name, :email, :age])
        |> validate_required([:name, :email, :age])
      end
    end
    ```

## Database Migrations

Migrations are used to modify the database schema over time. Generate a migration file using the `mix ecto.gen.migration` task.

1. Generate a migration:
    ```sh
    mix ecto.gen.migration create_users
    ```

2. Edit the generated file in `priv/repo/migrations/`:
    ```elixir
    defmodule MyApp.Repo.Migrations.CreateUsers do
      use Ecto.Migration

      def change do
        create table(:users) do
          add :name, :string
          add :email, :string
          add :age, :integer
          timestamps()
        end

        create unique_index(:users, [:email])
      end
    end
    ```

3. Run the migration:
    ```sh
    mix ecto.migrate
    ```

## Writing Queries

Ecto provides a powerful query DSL to interact with your database. You can write queries using the `Ecto.Query` module.

1. Basic query example:
    ```elixir
    import Ecto.Query
    alias MyApp.Repo
    alias MyApp.User

    # Fetch all users
    users = Repo.all(User)

    # Fetch a user by ID
    user = Repo.get(User, 1)

    # Fetch users with specific criteria
    query = from u in User, where: u.age > 30
    users_above_30 = Repo.all(query)
    ```

2. Inserting data:
    ```elixir
    %User{name: "John", email: "john@example.com", age: 25}
    |> Repo.insert()
    ```

3. Updating data:
    ```elixir
    user = Repo.get(User, 1)
    changeset = User.changeset(user, %{age: 26})
    Repo.update(changeset)
    ```

4. Deleting data:
    ```elixir
    user = Repo.get(User, 1)
    Repo.delete(user)
    ```

## Changesets and Data Validation

Changesets allow you to cast and validate data before performing database operations. They are essential for ensuring data integrity.

1. Creating a changeset:
    ```elixir
    def changeset(user, attrs) do
      user
      |> cast(attrs, [:name, :email, :age])
      |> validate_required([:name, :email, :age])
      |> validate_format(:email, ~r/@/)
      |> validate_number(:age, greater_than: 0)
    end
    ```

2. Using a changeset for insertion:
    ```elixir
    attrs = %{name: "Jane", email: "jane@example.com", age: 28}
    changeset = User.changeset(%User{}, attrs)

    case Repo.insert(changeset) do
      {:ok, user} -> IO.puts("User created: #{user.name}")
      {:error, changeset} -> IO.inspect(changeset.errors)
    end
    ```

## Conclusion

Ecto is a comprehensive toolkit for database interactions in Elixir. It simplifies defining schemas, managing database migrations, writing queries, and ensuring data integrity through changesets. By following this tutorial, you can efficiently manage your database operations in Elixir, making your application robust and maintainable.

Happy coding!

---

This guide should provide a solid foundation for using Ecto in your Elixir projects.