package experiments

object EitherTransformer extends scala.App {

  import cats.data.EitherT
  import cats.instances.option._

  sealed trait AppError

  case object Unrecoverable extends AppError
  case object Recoverable   extends AppError

  EitherT.fromEither[Option]
    .apply[AppError, Int](Left(Recoverable))
    .leftSemiflatMap {
      case Unrecoverable => None
      case Recoverable   => Some("Success")
    }

  val unrecoverableError: EitherT[Option, String, Int] = EitherT.fromEither[Option]
    .apply[AppError, Int](Left(Unrecoverable))
    .leftSemiflatMap {
      case Unrecoverable => None
      case Recoverable   => Some("55")
    }

  val successValue: EitherT[Option, String, Int] = EitherT.fromEither[Option]
    .apply[AppError, Int](Right(10_000))
    .leftSemiflatMap {
      case Unrecoverable => None
      case Recoverable   => Some("100")
    }

  println(unrecoverableError.value)
  println(successValue.value)

}
