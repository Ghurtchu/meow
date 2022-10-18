package part2math

import MyFunctorImpl.FunctorInstances._
import MyFunctorImpl.Syntax._

object MyFunctorImpl {

  trait Functor[F[_]] {
    def map[A, B](value: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_]](implicit functor: Functor[F]): Functor[F] = functor
  }

  object FunctorInstances {

    implicit val optionFunctor: Functor[Option] = new Functor[Option] {
      override def map[A, B](value: Option[A])(f: A => B): Option[B] = value.map(f)
    }

    implicit val listFunctor: Functor[List] = new Functor[List] {
      override def map[A, B](value: List[A])(f: A => B): List[B] = value.map(f)
    }

  }

  object Syntax {
    final implicit class FunctorSyntax[A, F[_]: Functor](value: F[A]) {
      def ~>[B](f: A => B): F[B] = Functor[F].map(value)(f)
    }
  }

  def main(args: Array[String]): Unit = {
    println(Option(1) ~> (_ + 1))
    println(Functor[Option].map(Option("Yeah"))(_.toUpperCase))
  }

}
