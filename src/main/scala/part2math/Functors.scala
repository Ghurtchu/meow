package part2math

import scala.util.Try

object Functors {

  // Functor provides a `map` method

  val increment: Int => Int = _ + 1
  val aModifiedList   = List(1, 2, 3).map(increment) // 2, 3, 4
  val aModifiedOption = Option(2).map(increment) // Some(3)
  val aModifiedTry    = Try(42).map(increment) // Success(43)

  // higher-kinded type class
  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor // functor type class
  import cats.instances.list._ // type class instances for list including functor list

  val listFunctor = Functor[List] // Functor type class instance for list
  val incrementedNumbers = listFunctor.map(1 :: 2 :: 3 :: 4 :: Nil)(increment)

  import cats.instances.option._ // includes Functor[Option]

  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(5))(increment) // Some(6)

  import cats.instances.try_._

  val tryFunctor = Functor[Try]
  val incrementedTry = tryFunctor.map(Try(42))(increment) // Success(43)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(op: Option[Int]): Option[Int] = op.map(_ * 10)


  // general solution
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  def map[F[_], A, B](fa: F[A], f: A => B)(implicit functor: Functor[F]): F[B] = functor.map(fa)(f)

  trait Tree[+A]

  object Tree {

    final case class Leaf[+A](value: A)                                  extends Tree[A]
    final case class Branch[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

    // for type inference for type classes
    def leaf[A](value: A): Tree[A] = Leaf(value)
    def branch[A](value: A, left: Tree[A], right: Tree[A]): Tree[A] = Branch(value, left, right)
  }

  implicit object treeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Tree.Branch(value, left, right) => Tree.Branch(f(value), map(left)(f), map(right)(f))
        case Tree.Leaf(value) => Tree.Leaf(f(value))
      }
    }
  }

  // extension methods
  import cats.syntax.functor._

  val tree: Tree[Int] = Tree.branch(40, Tree.leaf(41), Tree.leaf(42))
  val incrementedTree = tree.map[Int](increment) // extension from Functor[Tree]


  def main(args: Array[String]): Unit = {
    println(incrementedNumbers)
    println(incrementedOption)
    println(incrementedTry)

    println {
      do10x(List(1, 2, 3))
    }

    println {
      do10x(Option(5))
    }


    // type with "Tree" because type classes are invariant, so we must type it explicitly with `Tree`!
    println {
      do10x[Tree](Tree.Branch(1, Tree.Leaf(2), Tree.Leaf(3)))
    }

    println { // smart constructors are useful for type inference
      do10x(Tree.branch(1, Tree.leaf(2), Tree.leaf(3)))
    }

    println(incrementedTree)

    println {
      do10xShorter(Tree.branch(1, Tree.leaf(2), Tree.leaf(3)))
    }

    println(map[Option, Int, String](Option(5), _.toString))

  }

  // TODO: do10x shorter
  def do10xShorter[F[_]: Functor](container: F[Int]): F[Int] = {
    container.map(_ * 10)
  }

}
