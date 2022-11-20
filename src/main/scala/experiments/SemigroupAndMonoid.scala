package experiments

object SemigroupAndMonoid {

  trait Semigroup[A] {
    def combine(a1: A, a2: A): A
  }

  object Semigroup {
    def apply[A](implicit semigroup: Semigroup[A]): Semigroup[A] = semigroup // for summoning existing instance
    def instance[A](op: (A, A) => A): Semigroup[A] = op(_, _) // for constructing instance
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid // for summoning existing instance
    def instance[A](zero: A, op: (A, A) => A): Monoid[A] = new Monoid[A] {
      override def empty: A = zero
      override def combine(a1: A, a2: A): A = op(a1, a2)
    }
  }

  implicit class SemigroupSyntax[A: Semigroup](self: A) {
    def |+|(that: A): A = Semigroup[A].combine(self, that)
  }

  implicit class MonoidSyntax[A: Monoid](self: A) {
    def zero: A = Monoid[A].empty
  }

  def reduce[A: Monoid](as: List[A]): A = as.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  def main(args: Array[String]): Unit = {

    implicit val intMonoid: Monoid[Int] = Monoid.instance[Int](0, _ + _)

    println(reduce(1 :: 2 :: 3 :: Nil))

  }
}
