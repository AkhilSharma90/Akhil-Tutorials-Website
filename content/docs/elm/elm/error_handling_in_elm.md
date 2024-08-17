---
title: "Error Handling in Elm"
description: "Handling HTTP requests and JSON data in Elm."
icon: "code"
draft: false
---

An error is any unexpected condition in a program. Errors can occur at either compile-time or runtime. Compile time errors occur during the compilation of a program (For example, error in the program's syntax) while runtime errors occur during the program's execution. Unlike other programming languages, Elm does not throw runtime errors.

Consider an application that accepts the age of a user. The application should throw an error if the age is zero or negative. In this case, the Elm application can use the concept of error handling to explicitly raise an error at runtime if the user enters zero or a negative value as age. Error handling specifies the course of action if anything unexpected happens during the program's execution.

Elm programming language handles errors in the following ways − MayBe and Result

### MayBe

Consider the search feature in an application. The search function returns related data if the search keyword is found else does not return anything. This use case can be implemented in Elm using the MayBe type.

**Syntax**

```elm
variable_name:MayBe data_type
```

A variable of type MayBe can contain either of the following values −

- Just some_Value − This is used if there is valid data.
- Nothing − This is used if the value is absent or unknown. Nothing is equivalent to null in other programming languages.

**Illustration**
The following example shows how to use MayBe type with variables and function.

Step 1 − Create a MayBeDemo.elm file and add the following code to it

```elm
-- MayBeDemo.elm
module MayBeDemo exposing(..)
import Maybe

--declaring a MayBe variable and assigning value to it
userName : Maybe String
userName = Just "Mohtashim"

--declaring a MayBe variable and assigning value to it
userAge :Maybe Int
userAge = Just 20

--declaring a MayBe variable and assigning value to it
userSalary:Maybe Float
userSalary = Nothing

--declaring a custom type
type Country = India | China | SriLanka

--defining a function that takes a String parameter as input and returns a value of type MayBe

getCountryFromString : String -> Maybe Country
getCountryFromString p =
case p of
   "India"
      -> Just India
   "China"
      -> Just China
   "SriLanka"
      -> Just SriLanka
   _
      -> Nothing
```

Step 2 − Import the module in elm repl and execute as given below

```elm
E:\ElmWorks\ErroApp> elm repl
---- elm-repl 0.18.0 -----------------------------------------------------------
:help for help, :exit to exit, more at
--------------------------------------------------------------------------------
> import MayBeDemo exposing(..)
> userName
Just "Mohtashim" : Maybe.Maybe String
> userAge
Just 20 : Maybe.Maybe Int
> userSalary
Nothing : Maybe.Maybe Float
> getCountryFromString "India"
Just India : Maybe.Maybe MayBeDemo.Country
> getCountryFromString "india"
Nothing : Maybe.Maybe MayBeDemo.Country
```

The function checks if the value passed to the function is India or China or SriLanka. If the parameter's value does not match any of these, it returns nothing.

### Result

Consider an example, where the application needs to validate some condition and raise an error if the condition is not satisfied. The Result type can be used to achieve this. The Result type should be used if the application wants to explicitly raise an error and return details about what went wrong.

**Syntax**
The Result type declaration takes two parameters – the data type of the error (usually String) and the data type of the result to be returned if everything goes fine.

```elm
type Result error_type data_value_type
= Ok data_value
| Err error_message
```

The Result type returns either of the following values −

- Ok some_value − Represents result to be returned
- Err − Represents the error message to be returned if the expected conditions are not satisfied.

Illustration 1
Try the following example in the Elm REPL −

```elm
> String.toInt
<function> : String -> Result.Result String Int
-- successful result
> String.toInt "10"
Ok 10 : Result.Result String Int
-- unsuccessful result , Error
> String.toInt "a"
Err "could not convert string 'a' to an Int" : Result.Result String Int
```

The String.toInt function returns Integer value if the parameter passed is valid. If the parameter is not a number, the function returns an error.

Illustration 2
The following example accepts age as a parameter. The function returns the age if it is between 0 and 135 else it returns an appropriate error message.

Step 1 − Create a ResultDemo.elm file and add the following code to it.

```elm
--ResultDemo.elm
module ResultDemo exposing(..)

userId : Result String Int
userId = Ok 10

emailId : Result String Int
emailId = Err "Not valid emailId"

isReasonableAge : String -> Result String Int
isReasonableAge input =
   case String.toInt input of
      Err r ->
         Err "That is not a age!"

   Ok age ->
      if age < 0 then
         Err "Please try again ,age can't be negative"
      else if age > 135 then
         Err "Please try agian,age can't be this big.."

   else
      Ok age
```

Step 2 − Import the module in elm package and execute as given below

```elm
E:\ElmWorks\ElmRepo\15_ErrorHandling\15_Code> elm repl
---- elm-repl 0.18.0 -----------------------------------------------------------
:help for help, :exit to exit, more at <https://github.com/elm-lang/elm-repl>
--------------------------------------------------------------------------------
> import ResultDemo exposing (..)
> userId
Ok 10 : Result.Result String Int
> emailId
Err "Not valid emailId" : Result.Result String Int
> isReasonableAge "10"
Ok 10 : Result.Result String Int
> isReasonableAge "abc"
Err "That is not a age!" : Result.Result String Int
```

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
