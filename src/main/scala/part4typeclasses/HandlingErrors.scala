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

  def main(args: Array[String]): Unit = {

  }

}
