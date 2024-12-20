import scala.language.postfixOps
import scala.io.StdIn
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}
import java.{util => ju}

/** Contains basic data types, data structures and `Future` extensions.
  */
package object nodescala {

  /** Adds extensions methods to the `Future` companion object.
    */
  implicit class FutureCompanionOps(val future: Future.type) extends AnyVal {

    /** Returns a future that is always completed with `value`.
      */
    def always[T](value: T): Future[T] = Promise[T]().success(value).future

    /** Returns a future that is never completed.
      *
      * This future may be useful when testing if timeout logic works correctly.
      */
    def never[T]: Future[T] = Promise[T]().future

    /** Given a list of futures `futures`, returns the future holding the list
      * of values of all the futures from `futures`. The returned future is
      * completed only once all of the futures in `futures` have been completed.
      * The values in the list are in the same order as corresponding futures
      * `futures`. If any of the futures `futures` fails, the resulting future
      * also fails.
      */
    def all[T](futures: List[Future[T]]): Future[List[T]] =
      Future.sequence(futures)

    /** Given a list of futures `futures`, returns the future holding the value
      * of the future from `futures` that completed first. If the first
      * completing future in `futures` fails, then the result is failed as well.
      *
      * E.g.:
      *
      * Future.any(List(Future { 1 }, Future { 2 }, Future { throw new Exception
      * }))
      *
      * may return a `Future` succeeded with `1`, `2` or failed with an
      * `Exception`.
      */
    def any[T](futures: List[Future[T]]): Future[T] = {
      val promise = Promise[T]()
      for (future <- futures) yield promise.completeWith(future)
      promise.future
    }

    /** Returns a future with a unit value that is completed after time `time`.
      */
    def delay(time: Duration): Future[Unit] = Future {
      try {
        Await.ready(never, time)
        ()
      } catch {
        case _: Throwable => ()
      }
    }

    /** Completes this future with user input.
      */
    def userInput(message: String): Future[String] = Future {
      blocking {
        StdIn.readLine(message)
      }
    }

    /** Creates a cancellable context for an execution and runs it.
      */
    def run()(f: CancellationToken => Future[Unit]): Subscription = {
      val source = CancellationTokenSource()
      f(source.cancellationToken)
      source
    }
  }

  /** Adds extension methods to future objects.
    */
  implicit class FutureOps[T](val future: Future[T]) extends AnyVal {

    /** Returns the result of this future if it is completed now. Otherwise,
      * throws a `NoSuchElementException`.
      *
      * Note: This method does not wait for the result. It is thus non-blocking.
      * However, it is also non-deterministic -- it may throw or return a value
      * depending on the current state of the `Future`.
      */
    def now: T =
      try {
        Await.result(future, Duration.Zero)
      } catch {
        case _: Throwable => throw new NoSuchElementException()
      }

    /** Continues the computation of this future by taking the current future
      * and mapping it into another future.
      *
      * The function `continue` is called only after the current future
      * completes. The resulting future contains a value returned by `continue`.
      */
    def continueWith[S](continue: Future[T] => S): Future[S] =
      future collect {
        case Success(value)     => continue(future)
        case Failure(exception) => throw exception
      }

    /** Continues the computation of this future by taking the result of the
      * current future and mapping it into another future.
      *
      * The function `continue` is called only after the current future
      * completes. The resulting future contains a value returned by `continue`.
      */
    def continue[S](continue: Try[T] => S): Future[S] =
      future.map(value => continue(Success[T](value)))

  }

  /** Subscription objects are used to be able to unsubscribe from some event
    * source.
    */
  trait Subscription {
    def unsubscribe(): Unit
  }

  object Subscription {

    /** Given two subscriptions `s1` and `s2` returns a new composite
      * subscription such that when the new composite subscription cancels both
      * `s1` and `s2` when `unsubscribe` is called.
      */
    def apply(s1: Subscription, s2: Subscription) = new Subscription {
      def unsubscribe(): Unit = {
        s1.unsubscribe()
        s2.unsubscribe()
      }
    }
  }

  /** Used to check if cancellation was requested.
    */
  trait CancellationToken {
    def isCancelled: Boolean
    def nonCancelled = !isCancelled
  }

  /** The `CancellationTokenSource` is a special kind of `Subscription` that
    * returns a `cancellationToken` which is cancelled by calling `unsubscribe`.
    *
    * After calling `unsubscribe` once, the associated `cancellationToken` will
    * forever remain cancelled -- its `isCancelled` will return `false.
    */
  trait CancellationTokenSource extends Subscription {
    def cancellationToken: CancellationToken
  }

  /** Creates cancellation token sources.
    */
  object CancellationTokenSource {

    /** Creates a new `CancellationTokenSource`.
      */
    def apply() = new CancellationTokenSource {
      val p = Promise[Unit]()
      val cancellationToken = new CancellationToken {
        def isCancelled = p.future.value != None
      }
      def unsubscribe(): Unit = {
        p.trySuccess(())
      }
    }
  }
}
