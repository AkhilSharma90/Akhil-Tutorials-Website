---
title: "Testing in Elixir with ExUnit"
description: ""
icon: "code"
draft: false
---

Testing is a crucial aspect of software development that ensures code reliability and correctness. Elixir provides a powerful testing framework, ExUnit, which comes with a rich set of features for writing and running tests. This tutorial will cover the basics of ExUnit, writing test cases, using mocks and stubs, and property-based testing with StreamData.

## Introduction to ExUnit

ExUnit is Elixir's built-in test framework, providing a comprehensive suite of tools to write and execute tests. It includes features like assertions, refutations, setup callbacks, and more.

## Setting Up ExUnit

ExUnit is included by default in new Elixir projects. To start using ExUnit, ensure that your project is properly set up by following these steps:

1. Create a new Elixir project:

   ```sh
   mix new my_project
   cd my_project
   ```

2. Ensure ExUnit starts automatically by adding the following to your `test/test_helper.exs` file:
   ```elixir
   ExUnit.start()
   ```

## Writing Test Cases

A basic test case in ExUnit is defined using the `test` macro within a `defmodule`. Here's an example:

1. Create a test file `test/my_project_test.exs`:

   ```elixir
   defmodule MyProjectTest do
     use ExUnit.Case
     doctest MyProject

     test "the truth" do
       assert 1 + 1 == 2
     end
   end
   ```

2. Run the tests:
   ```sh
   mix test
   ```

### Assertions and Refutations

ExUnit provides several macros for assertions and refutations:

- `assert`: Asserts that an expression evaluates to true.
- `refute`: Asserts that an expression evaluates to false.

Example:

```elixir
test "assertions and refutations" do
  assert String.length("hello") == 5
  refute String.length("hello") == 4
end
```

## Structuring Test Suites

Organize tests in separate modules and use setup callbacks for shared test setup:

1. Create a file `test/user_test.exs`:

   ```elixir
   defmodule UserTest do
     use ExUnit.Case

     setup do
       {:ok, user: %User{name: "John", age: 30}}
     end

     test "user has a name", %{user: user} do
       assert user.name == "John"
     end
   end
   ```

## Mocks and Stubs

Using mocks and stubs allows you to isolate tests by replacing external dependencies with controlled behavior. The `Mox` library is commonly used in Elixir for this purpose.

1. Add `mox` to your `mix.exs` dependencies:

   ```elixir
   defp deps do
     [
       {:mox, "~> 1.0"}
     ]
   end
   ```

2. Define a behaviour and create a mock in `test/support/mocks.ex`:

   ```elixir
   defmodule MyApp.ServiceBehaviour do
     @callback call(any()) :: any()
   end

   Mox.defmock(MyApp.ServiceMock, for: MyApp.ServiceBehaviour)
   ```

3. Use the mock in your tests:

   ```elixir
   defmodule MyApp.ServiceTest do
     use ExUnit.Case, async: true
     import Mox

     setup :verify_on_exit!

     test "service call" do
       MyApp.ServiceMock
       |> expect(:call, fn _ -> :ok end)

       assert MyApp.Service.call(:arg) == :ok
     end
   end
   ```

## Property-based Testing with StreamData

Property-based testing is a powerful technique where properties (general truths) about the code are defined and tested with many different inputs. The `StreamData` library facilitates this in Elixir.

1. Add `stream_data` to your `mix.exs` dependencies:

   ```elixir
   defp deps do
     [
       {:stream_data, "~> 0.5"}
     ]
   end
   ```

2. Write property-based tests:

   ```elixir
   defmodule MyApp.PropertyTest do
     use ExUnit.Case
     use ExUnitProperties

     property "list reversal" do
       check all list <- list_of(integer()) do
         assert Enum.reverse(Enum.reverse(list)) == list
       end
     end
   end
   ```

## Conclusion

ExUnit provides a robust and flexible framework for testing Elixir applications. By leveraging its features, you can ensure your code is reliable, maintainable, and free of bugs. Integrating additional tools like Mox and StreamData further enhances your testing capabilities, enabling comprehensive and thorough test coverage.

Happy testing!

---

This comprehensive guide should serve as a useful resource for developers looking to improve their Elixir testing practices.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
