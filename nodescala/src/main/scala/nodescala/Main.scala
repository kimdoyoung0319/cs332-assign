package nodescala

import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}

object Main {

  def main(args: Array[String]): Unit = {
    // 1. instantiate the server at 8191, relative path "/test",
    //    and have the response return headers of the request
    val myServer = new NodeScala.Default(8191)
    val myServerSubscription = myServer.start("/test") { request =>
      for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // 2. create a future that expects some user input `x`
    //    and continues with a `"You entered... " + x` message
    val userInterrupted =
      Future.userInput("Hit ENTER to cancel... ") continueWith { f =>
        "You entered... " + f.now
      }

    // 3. create a future that completes after 20 seconds
    //    and continues with a `"Server timeout!"` message
    val timeOut: Future[String] =
      Future delay (20 seconds) collect { case _ => "Server timeout!" }

    // 4. create a future that completes when either 20 seconds elapse
    //    or the user enters some text and presses ENTER
    val terminationRequested: Future[String] = {
      val promise = Promise[String]()
      timeOut foreach { value => promise.trySuccess(value) }
      userInterrupted foreach { value => promise.trySuccess(value) }
      promise.future
    }

    // 5. unsubscribe from the server
    terminationRequested onSuccess { case msg =>
      println(msg); myServerSubscription.unsubscribe()
    }
  }

}
