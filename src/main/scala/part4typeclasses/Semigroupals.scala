package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object Semigroupals {

  // basically ZIO#zip
  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._

  def main(args: Array[String]): Unit = {

    val opt: Semigroupal[Option] = Semigroupal[Option]
    val aTupledOption = opt.product(Some("Hello"), Some("World")) // Some(("Hello", "World"))
    val aNoneTupled = opt.product(Some(123), None) // None

    import cats.instances.future._
    import scala.concurrent.Future

    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    val aTupledFuture = Semigroupal[Future].product(Future("meaning of life"), Future(42)) // Future(("meaning of life", 42))



  }

}
