---
title: "Connecting to SQL Databases Using Erlang's ODBC Library"
description: ""
icon: "code"
draft: false
---

## Prerequisites

Before connecting to a Microsoft SQL Server database, ensure the following:

- You have created a database `TESTDB`.
- You have created a table `EMPLOYEE` in `TESTDB` with fields `FIRST_NAME`, `LAST_NAME`, `AGE`, `SEX`, and `INCOME`.
- User ID `testuser` and password `test123` are set to access `TESTDB`.
- An ODBC DSN named `usersqlserver` has been created, which connects to the database.

## Establishing a Database Connection

To establish a connection to the database, use the following code example:

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=usersqlserver;UID=testuser;PWD=test123", []),
    io:fwrite("~p", [Ref]).
```

### Output

```plaintext
<0.33.0>
```

### Explanation

- `odbc:start/0`: Initializes the ODBC application.
- `odbc:connect/2`: Establishes a connection using the provided DSN, user ID, and password.
- `io:fwrite/2`: Prints the reference to the established connection.

## Creating a Database Table

After connecting to the database, you can create tables. Here is an example:

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=usersqlserver;UID=testuser;PWD=test123", []),
    odbc:sql_query(Ref, "CREATE TABLE EMPLOYEE (FIRST_NAME VARCHAR(20), LAST_NAME VARCHAR(20), AGE INTEGER, SEX CHAR(1), INCOME INTEGER)").
```

## Inserting Records

To insert a record into the `EMPLOYEE` table, use the following code:

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=usersqlserver;UID=testuser;PWD=test123", []),
    io:fwrite("~p", [odbc:sql_query(Ref, "INSERT INTO EMPLOYEE VALUES('Mac', 'Mohan', 20, 'M', 2000)")]).
```

### Output

```plaintext
{updated,1}
```

## Fetching Records

To fetch records from the `EMPLOYEE` table, use the following code:

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=usersqlserver;UID=testuser;PWD=test123", []),
    io:fwrite("~p", [odbc:sql_query(Ref, "SELECT * FROM EMPLOYEE")]).
```

### Output

```plaintext
{selected,["FIRST_NAME","LAST_NAME","AGE","SEX","INCOME"],[{"Mac","Mohan",20,"M",2000}]}
```

## Fetching Records with Parameters

To fetch records based on certain criteria, use parameterized queries:

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=usersqlserver;UID=testuser;PWD=test123", []),
    io:fwrite("~p", [odbc:param_query(Ref, "SELECT * FROM EMPLOYEE WHERE SEX=?", [{{sql_char, 1}, ["M"]}])]).
```

### Output

```plaintext
{selected,["FIRST_NAME","LAST_NAME","AGE","SEX","INCOME"],[{"Mac","Mohan",20,"M",2000}]}
```

## Updating Records

To update records in the `EMPLOYEE` table, use the following code:

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=usersqlserver;UID=testuser;PWD=test123", []),
    io:fwrite("~p", [odbc:sql_query(Ref, "UPDATE EMPLOYEE SET AGE = 5 WHERE INCOME = 2000")]).
```

### Output

```plaintext
{updated,1}
```

## Deleting Records

To delete records from the `EMPLOYEE` table, use the following code:

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=usersqlserver;UID=testuser;PWD=test123", []),
    io:fwrite("~p", [odbc:sql_query(Ref, "DELETE FROM EMPLOYEE WHERE INCOME = 2000")]).
```

### Output

```plaintext
{updated,1}
```

## Describing a Table Structure

To describe the structure of the `EMPLOYEE` table, use the following code:

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=usersqlserver;UID=testuser;PWD=test123", []),
    io:fwrite("~p", [odbc:describe_table(Ref, "EMPLOYEE")]).
```

### Output

```plaintext
{ok,[{"FIRST_NAME",{sql_varchar,20}},
     {"LAST_NAME",{sql_varchar,20}},
     {"AGE",sql_integer},
     {"SEX",{sql_char,1}},
     {"INCOME",sql_integer}]}
```

## Fetching Record Count

To fetch the total count of records in the `EMPLOYEE` table, use the following code:

### Example

```erlang
-module(helloworld).
-export([start/0]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=usersqlserver;UID=testuser;PWD=test123", []),
    io:fwrite("~p", [odbc:select_count(Ref, "SELECT * FROM EMPLOYEE")]).
```

### Output

```plaintext
{ok,1}
```

## Conclusion

Erlang's ODBC library provides robust capabilities for interacting with SQL databases. By following the examples provided, you can establish connections, create tables, and perform CRUD (Create, Read, Update, Delete) operations on your database.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
