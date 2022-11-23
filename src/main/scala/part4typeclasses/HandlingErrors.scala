package part4typeclasses

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

  def main(args: Array[String]): Unit = {

  }

}
