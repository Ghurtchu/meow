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

    import cats.instances.list._

    val aTupledList = Semigroupal[List].product(1 :: 2 :: Nil, "a" :: "b" :: Nil)
    println(aTupledList)
    println {
      for {
        a <- 1 :: 2 :: Nil
        b <- "a" :: "b" :: Nil
      } yield (a, b)
    }

    // TODO
    import cats.Monad
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    def productWithMonadsBeautiful[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      for {
        a <- fa
        b <- fb
      } yield (a, b)


    def productWithMonadsUgly[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      Monad[F].flatMap(fa)(a => Monad[F].map(fb)(b => (a, b)))

  }

}
