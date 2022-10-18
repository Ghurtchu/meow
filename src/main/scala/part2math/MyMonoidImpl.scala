package part2math

import MySemigroupImpl._
import MyMonoidImpl.Syntax._
import MyMonoidImpl.MonoidInstances._

object MyMonoidImpl {

  trait Monoid[A] extends Semigroup[A] {
    def zero: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  }

  object MonoidInstances {

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      override def zero: Int = 0
      override def combine(a1: Int, a2: Int): Int = a1 + a2
    }

    implicit val stringMonoid: Monoid[String] = new Monoid[String] {
      override def zero: String = ""
      override def combine(a1: String, a2: String): String = a1 concat a2
    }

  }

  object Syntax {
    final implicit class MonoidSyntax[A: Monoid](value: A) {
      def |+|(that: A): A = Monoid[A].combine(value, that)
      def zero: A = Monoid[A].zero
    }
  }

  def aggregate[A: Monoid](elems: Seq[A]): A = elems.fold(Monoid[A].zero)(Monoid[A].combine)

  def main(args: Array[String]): Unit = {

    println {
      1 |+| 2 |+| 3
    }

    println(aggregate(1 :: 2 :: 3 :: Nil))

  }

}
