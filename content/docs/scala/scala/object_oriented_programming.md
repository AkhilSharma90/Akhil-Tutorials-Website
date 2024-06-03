---
title: "Object-Oriented Programming in Scala"
description: "Scala Lang description"
icon: "code"
draft: false
---

## Object-Oriented Programming in Scala

Scala treats everything as objects and supports key object-oriented programming (OOP) concepts such as classes, objects, constructors, inheritance, and traits.

**Classes and Objects**

- **Classes:** A class in Scala is a blueprint for creating objects. It can contain fields and methods.
  ```scala
  class Person(var name: String, var age: Int) {
    def greet(): Unit = {
      println(s"Hello, my name is $name and I am $age years old.")
    }
  }
  ```

- **Objects:** An instance of a class is known as an object. You create an object using the `new` keyword.
  ```scala
  val person1 = new Person("Kanye", 300)
  person1.greet()
  ```

**Primary and Auxiliary Constructors**

- **Primary Constructor:** Defined in the class signature. It can have default values.
  ```scala
  class Person(var name: String = "Unknown", var age: Int = 0)
  ```

- **Auxiliary Constructor:** Allows you to have additional constructors. They must call the primary constructor.
  ```scala
  class Person(var name: String, var age: Int) {
    def this(name: String) = this(name, 0) // Auxiliary Constructor
  }
  ```

**Inheritance and Traits**

- **Inheritance:** Scala supports single inheritance, where a class can extend another class.
  ```scala
  class Employee(name: String, age: Int, var salary: Double) extends Person(name, age)
  ```

- **Traits:** Similar to interfaces in Java, traits are used to define object types by specifying the signature of the supported methods.
  ```scala
  trait Greeting {
    def greet(message: String): Unit
  }
  class Person(var name: String) extends Greeting {
    def greet(message: String): Unit = println(s"$name says: $message")
  }
  ```

**Code Example: Object-Oriented Scala Program**

Here's an example combining these OOP concepts:

```scala
trait Greeting {
  def greet(): Unit
}

class Person(var name: String, var age: Int) extends Greeting {
  override def greet(): Unit = {
    println(s"Hello, I'm $name and I am $age years old.")
  }
}

object MainApp {
  def main(args: Array[String]): Unit = {
    val person1 = new Person("Alice", 25)
    person1.greet()
  }
}
```

This program defines a `Person` class with a trait `Greeting`. When we create an instance of `Person`, it can use the `greet` method from the trait.

