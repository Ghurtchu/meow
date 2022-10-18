package part2math

import MyFunctorImpl.Functor

object MyMonadImpl {

  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]
  }

  object Monad {
    def apply[F[_]](implicit monad: Monad[F]): Monad[F] = monad
  }

  object MonadInstances {

    implicit val optionMonad: Monad[Option] = new Monad[Option] {
      override def flatMap[A, B](value: Option[A])(f: A => Option[B]): Option[B] = value.flatMap(f)
      override def map[A, B](value: Option[A])(f: A => B): Option[B] = value.map(f)
    }

    implicit val listMonad: Monad[List] = new Monad[List] {
      override def flatMap[A, B](value: List[A])(f: A => List[B]): List[B] = value.flatMap(f)
      override def map[A, B](value: List[A])(f: A => B): List[B] = value.map(f)
    }

  }

  object Syntax {
    final implicit class MonadSyntax[A, F[_]: Monad](value: F[A]) {
      def ~>[B](f: A => B): F[B]     = Monad[F].map(value)(f)
      def ~~>[B](f: A => F[B]): F[B] = Monad[F].flatMap(value)(f)
    }
  }

  def main(args: Array[String]): Unit = {

    import MyMonadImpl.MonadInstances._
    import MyMonadImpl.Syntax._

    println(Option(5) ~> (_ + 1))
    println(Option("Rock the JVM") ~~> (s => Option(s.toUpperCase)))

  }

}
