package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object HandlingErrors {

  // levels of error-handling:
  // - try/catch blocks
  // - using Try / Either
  // - pure FP with Cats

  import cats.Monad
  // F[_] = Option
  // E = Error type
  trait MyMonadError[F[_], E] extends Monad[F] {
    def raiseError[A](error: E): F[A] = ???
  }

  import cats.MonadError

  import cats.instances.either._ // implicit MonadError

  type ErrorOr[A] = Either[String, A] // may fail with String or succeed with A
  val monadErrorEither = MonadError[ErrorOr, String] // must be same as Either[String, _]

  val success = monadErrorEither.pure[Int](32) // Either[String, Int] => Right(32)
  val failure = monadErrorEither.raiseError[Int]("Something wrong") // Left("Something wrong")

  val handled: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _         => 99
  }

  val handled2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "wtf" => Right(44)
    case _     => Right(100)
  }

  // "filter" API
  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100)

  // Try and Future
  import cats.instances.try_._ // implicit MonadError[Try]

  import scala.util.Try

  val exception = new RuntimeException("Boom")
  val pureException = MonadError[Try, Throwable].raiseError[Throwable](exception) // Try[Nothing] => Failure(Exception)

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))

  // applicatives => ApplicativeError
  import cats.data.Validated

  type ErrorsOr[A] = Validated[List[String], A]
  import cats.ApplicativeError

  import cats.instances.list._ // => ApplicativeError[ErrorsOr, List[String]]

  val appErrVal = ApplicativeError[ErrorsOr, List[String]]

  import cats.Applicative

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](err: E): F[A] // looks like pure
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A] // looks like flatMap
    def handleError[A](fa: F[A])(f: E => A): F[A] = handleErrorWith(fa)(e => pure(f(e))) // looks like map
  }

  def main(args: Array[String]): Unit = {

  }

}
