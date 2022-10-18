package part2math

import MySemigroupImpl.Semigroup // 1 - type class import
import MySemigroupImpl.SemigroupInstances._ // 2 - instances import of type class
import MySemigroupImpl.Syntax._

object MySemigroupImpl {

  trait Semigroup[A] {
    def combine(a1: A, a2: A): A
  }

  object Semigroup {
    def apply[A](implicit semigroup: Semigroup[A]): Semigroup[A] = semigroup
  }

  object SemigroupInstances {
    implicit val intSemigroup: Semigroup[Int] = _ + _
    implicit val stringSemigroup: Semigroup[String] = _ concat _
    implicit val listSemigroup: Semigroup[List[Any]] = _ ::: _
  }

  // 4 - extension methods

  object Syntax {
    final implicit class SemigroupSyntax[A](value: A) {
      def |+|(that: A)(implicit semigroup: Semigroup[A]): A = semigroup.combine(value, that)
    }
  }

  def main(args: Array[String]): Unit = {
    // 3 - use the type class API
    println(Semigroup[String].combine("a1", "a2"))
    println("A1" |+| "A2")
    println(1 |+| 2)
    println()
  }

}
