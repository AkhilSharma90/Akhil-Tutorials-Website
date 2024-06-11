---
title: "Future and Promises in Scala"
description: "Futures provide a way to reason about performing many operations in parallel – in an efficient and non-blocking way."
icon: "code"
draft: false
---

A Future is a placeholder object for a value that may not yet exist. Generally, the value of the Future is supplied concurrently and can subsequently be used. Composing concurrent tasks in this way tends to result in faster, asynchronous, non-blocking parallel code.

By default, futures and promises are non-blocking, making use of callbacks instead of typical blocking operations. To simplify the use of callbacks both syntactically and conceptually, Scala provides combinators such as flatMap, foreach, and filter used to compose futures in a non-blocking way. Blocking is still possible - for cases where it is absolutely necessary, futures can be blocked on (although this is discouraged).

## Execution Context

Future and Promises revolve around ExecutionContexts, responsible for executing computations.

An ExecutionContext is similar to an Executor: it is free to execute computations in a new thread, in a pooled thread or in the current thread (although executing the computation in the current thread is discouraged – more on that below).

The scala.concurrent package comes out of the box with an ExecutionContext implementation, a global static thread pool. It is also possible to convert an Executor into an ExecutionContext. Finally, users are free to extend the ExecutionContext trait to implement their own execution contexts, although this should only be done in rare cases.

### The Global Execution Context

ExecutionContext.global is an ExecutionContext backed by a ForkJoinPool. It should be sufficient for most situations but requires some care. A ForkJoinPool manages a limited number of threads (the maximum number of threads being referred to as parallelism level). The number of concurrently blocking computations can exceed the parallelism level only if each blocking call is wrapped inside a blocking call (more on that below). Otherwise, there is a risk that the thread pool in the global execution context is starved, and no computation can proceed.

By default, the ExecutionContext.global sets the parallelism level of its underlying fork-join pool to the number of available processors (Runtime.availableProcessors). This configuration can be overridden by setting one (or more) of the following VM attributes:

scala.concurrent.context.minThreads - defaults to 1
scala.concurrent.context.numThreads - can be a number or a multiplier (N) in the form ‘xN’ ; defaults to Runtime.availableProcessors
scala.concurrent.context.maxThreads - defaults to Runtime.availableProcessors
The parallelism level will be set to numThreads as long as it remains within [minThreads; maxThreads].

As stated above the ForkJoinPool can increase the number of threads beyond its parallelismLevel in the presence of blocking computation. As explained in the ForkJoinPool API, this is only possible if the pool is explicitly notified:


```scala
import scala.concurrent.{ Future, ExecutionContext }
import scala.concurrent.forkjoin.*

// the following is equivalent to `given ExecutionContext = ExecutionContext.global`
import ExecutionContext.Implicits.global

Future {
  ForkJoinPool.managedBlock(
    new ManagedBlocker {
       var done = false

       def block(): Boolean =
         try
           myLock.lock()
           // ...
         finally
           done = true
         true

       def isReleasable: Boolean = done
    }
  )
}
```


Fortunately the concurrent package provides a convenient way for doing so:

```scala
import scala.concurrent.Future
import scala.concurrent.blocking

Future {
  blocking {
    myLock.lock()
    // ...
  }
}
```

Note that blocking is a general construct that will be discussed more in depth below.

## Futures
A Future is an object holding a value which may become available at some point. This value is usually the result of some other computation:

- If the computation has not yet completed, we say that the Future is not completed.
- If the computation has completed with a value or with an exception, we say that the Future is completed.

Completion can take one of two forms:

- When a Future is completed with a value, we say that the future was successfully completed with that value.
- When a Future is completed with an exception thrown by the computation, we say that the Future was failed with that exception.

A Future has an important property that it may only be assigned once. Once a Future object is given a value or an exception, it becomes in effect immutable – it can never be overwritten.

The simplest way to create a future object is to invoke the Future.apply method which starts an asynchronous computation and returns a future holding the result of that computation. The result becomes available once the future completes.

Note that Future[T] is a type which denotes future objects, whereas Future.apply is a method which creates and schedules an asynchronous computation, and then returns a future object which will be completed with the result of that computation.

This is best shown through an example.

Let’s assume that we want to use a hypothetical API of some popular social network to obtain a list of friends for a given user. We will open a new session and then send a request to obtain a list of friends of a particular user:

```scala
import scala.concurrent._
import ExecutionContext.Implicits.global

val session = socialNetwork.createSessionFor("user", credentials)
val f: Future[List[Friend]] = Future {
  session.getFriends()
}
import scala.concurrent.*
import ExecutionContext.Implicits.global

val session = socialNetwork.createSessionFor("user", credentials)
val f: Future[List[Friend]] = Future {
  session.getFriends()
}
```

Above, we first import the contents of the scala.concurrent package to make the type Future visible. We will explain the second import shortly.

We then initialize a session variable which we will use to send requests to the server, using a hypothetical createSessionFor method. To obtain the list of friends of a user, a request has to be sent over a network, which can take a long time. This is illustrated with the call to the method getFriends that returns List[Friend]. To better utilize the CPU until the response arrives, we should not block the rest of the program – this computation should be scheduled asynchronously. The Future.apply method does exactly that – it performs the specified computation block concurrently, in this case sending a request to the server and waiting for a response.

The list of friends becomes available in the future f once the server responds.

An unsuccessful attempt may result in an exception. In the following example, the session value is incorrectly initialized, so the computation in the Future block will throw a NullPointerException. This future f is then failed with this exception instead of being completed successfully:

```scala
val session = null
val f: Future[List[Friend]] = Future {
  session.getFriends()
}
```

The line import ExecutionContext.Implicits.global above imports the default global execution context. Execution contexts execute tasks submitted to them, and you can think of execution contexts as thread pools. They are essential for the Future.apply method because they handle how and when the asynchronous computation is executed. You can define your own execution contexts and use them with Future, but for now it is sufficient to know that you can import the default execution context as shown above.

Our example was based on a hypothetical social network API where the computation consists of sending a network request and waiting for a response. It is fair to offer an example involving an asynchronous computation which you can try out of the box. Assume you have a text file, and you want to find the position of the first occurrence of a particular keyword. This computation may involve blocking while the file contents are being retrieved from the disk, so it makes sense to perform it concurrently with the rest of the computation.

```scala
val firstOccurrence: Future[Int] = Future {
  val source = scala.io.Source.fromFile("myText.txt")
  source.toSeq.indexOfSlice("myKeyword")
}
```

## Callbacks
We now know how to start an asynchronous computation to create a new future value, but we have not shown how to use the result once it becomes available, so that we can do something useful with it. We are often interested in the result of the computation, not just its side-effects.

In many future implementations, once the client of the future becomes interested in its result, it has to block its own computation and wait until the future is completed – only then can it use the value of the future to continue its own computation. Although this is allowed by the Scala Future API as we will show later, from a performance point of view a better way to do it is in a completely non-blocking way, by registering a callback on the future. This callback is called asynchronously once the future is completed. If the future has already been completed when registering the callback, then the callback may either be executed asynchronously, or sequentially on the same thread.

The most general form of registering a callback is by using the onComplete method, which takes a callback function of type Try[T] => U. The callback is applied to the value of type Success[T] if the future completes successfully, or to a value of type Failure[T] otherwise.

The Try[T] is similar to Option[T] or Either[T, S], in that it is a monad potentially holding a value of some type. However, it has been specifically designed to either hold a value or some throwable object. Where an Option[T] could either be a value (i.e. Some[T]) or no value at all (i.e. None), Try[T] is a Success[T] when it holds a value and otherwise Failure[T], which holds an exception. Failure[T] holds more information than just a plain None by saying why the value is not there. In the same time, you can think of Try[T] as a special version of Either[Throwable, T], specialized for the case when the left value is a Throwable.

Coming back to our social network example, let’s assume we want to fetch a list of our own recent posts and render them to the screen. We do so by calling a method getRecentPosts which returns a List[String] – a list of recent textual posts:

```scala
import scala.util.{Success, Failure}

val f: Future[List[String]] = Future {
  session.getRecentPosts()
}

f.onComplete {
  case Success(posts) => for post <- posts do println(post)
  case Failure(t) => println("An error has occurred: " + t.getMessage)
}
```

The onComplete method is general in the sense that it allows the client to handle the result of both failed and successful future computations. In the case where only successful results need to be handled, the foreach callback can be used:

```scala
val f: Future[List[String]] = Future {
  session.getRecentPosts()
}

for {
  posts <- f
  post <- posts
} println(post)
val f: Future[List[String]] = Future {
  session.getRecentPosts()
}

for
  posts <- f
  post <- posts
do println(post)
```

Futures provide a clean way of handling only failed results using the failed projection which converts a Failure[Throwable] to a Success[Throwable]. An example of doing this is provided in the section below on projections.

Coming back to the previous example with searching for the first occurrence of a keyword, you might want to print the position of the keyword to the screen:

```scala
val firstOccurrence: Future[Int] = Future {
  val source = scala.io.Source.fromFile("myText.txt")
  source.toSeq.indexOfSlice("myKeyword")
}

firstOccurrence.onComplete {
  case Success(idx) => println("The keyword first appears at position: " + idx)
  case Failure(t) => println("Could not process file: " + t.getMessage)
}
```

The onComplete and foreach methods both have result type Unit, which means invocations of these methods cannot be chained. Note that this design is intentional, to avoid suggesting that chained invocations may imply an ordering on the execution of the registered callbacks (callbacks registered on the same future are unordered).

That said, we should now comment on when exactly the callback gets called. Since it requires the value in the future to be available, it can only be called after the future is completed. However, there is no guarantee it will be called by the thread that completed the future or the thread which created the callback. Instead, the callback is executed by some thread, at some time after the future object is completed. We say that the callback is executed eventually.

Furthermore, the order in which the callbacks are executed is not predefined, even between different runs of the same application. In fact, the callbacks may not be called sequentially one after the other, but may concurrently execute at the same time. This means that in the following example the variable totalA may not be set to the correct number of lower case and upper case a characters from the computed text.

```scala
@volatile var totalA = 0

val text = Future {
  "na" * 16 + "BATMAN!!!"
}

text.foreach { txt =>
  totalA += txt.count(_ == 'a')
}

text.foreach { txt =>
  totalA += txt.count(_ == 'A')
}
```
Above, the two callbacks may execute one after the other, in which case the variable totalA holds the expected value 18. However, they could also execute concurrently, so totalA could end up being either 16 or 2, since += is not an atomic operation (i.e. it consists of a read and a write step which may interleave arbitrarily with other reads and writes).

For the sake of completeness the semantics of callbacks are listed here:

Registering an onComplete callback on the future ensures that the corresponding closure is invoked after the future is completed, eventually.

Registering a foreach callback has the same semantics as onComplete, with the difference that the closure is only called if the future is completed successfully.

Registering a callback on the future which is already completed will result in the callback being executed eventually (as implied by 1).

In the event that multiple callbacks are registered on the future, the order in which they are executed is not defined. In fact, the callbacks may be executed concurrently with one another. However, a particular ExecutionContext implementation may result in a well-defined order.

In the event that some callbacks throw an exception, the other callbacks are executed regardless.

In the event that some callbacks never complete (e.g. the callback contains an infinite loop), the other callbacks may not be executed at all. In these cases, a potentially blocking callback must use the blocking construct (see below).

Once executed, the callbacks are removed from the future object, thus being eligible for GC.