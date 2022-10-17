package part2math

import cats.Semigroup
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._
import cats.syntax.semigroup._
import cats.instances.map._

// monoids are semigroups with zero value
import cats.Monoid

object Monoids extends scala.App {

  val numbers = (1 to 1000).toList
  // |+| is always associative (a + b) == (b + a)
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  // needs additional param "zero: A"
  def combineFold[A](list: List[A], zero: A)(implicit semigroup: Semigroup[A]): A =
    list.foldLeft(zero)(_ |+| _)

  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(1, 2) // zero equals to 0 for ints, "" for strings etc..
  val zero = intMonoid.empty
  println(zero) // number 0

  val emptyString = Monoid[String].empty
  println(emptyString)

  println(combineFold(1 :: 2 :: Nil, 0))

  assert(sumLeft == sumRight)
  println(sumLeft)
  println(sumRight)

  println(Monoid[Option[Int]].combine(Option(2), Option.empty[Int]))
  println(Monoid[Option[Int]].combine(Option(2), Option(3)))

  println(Option(2) |+| Option(5))

  // monoid
  def reduceWithMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.fold(monoid.empty)(monoid.combine)

  println {
    reduceWithMonoid {
      1 :: 2 :: 3 :: Nil
    }
  }

  // combine a list of phonebooks as Map[String, Int]
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Anzori" -> 657
    ),
    Map(
      "Nika" -> 372,
      "Daniel" -> 999
    ),
    Map("Tina" -> 123)
  )

  println(reduceWithMonoid(phonebooks))

  // ex 3 - shopping cart and online stores with monoids
  final case class ShoppingCart(items: List[String], total: Double)

  implicit lazy val shoppingCartMonoid: Monoid[ShoppingCart] = new Monoid[ShoppingCart] {
    override def empty: ShoppingCart = ShoppingCart(List.empty[String], 0.0)
    override def combine(x: ShoppingCart, y: ShoppingCart): ShoppingCart = ShoppingCart(
        items = x.items ::: y.items,
        total = x.total + y.total
      )
  }

  final case class A()

  implicit class ShoppingCartMonoidSyntax(shoppingCart: ShoppingCart) {
    def |+|(that: ShoppingCart): ShoppingCart = Monoid[ShoppingCart].combine(shoppingCart, that)
  }

  def checkout(shoppingCarts: List[ShoppingCart])(implicit monoid: Monoid[ShoppingCart]): ShoppingCart =
    shoppingCarts.fold(monoid.empty)(_ |+| _)

  println(checkout(ShoppingCart("Banana" :: "Shirt" :: Nil, 30.00) :: ShoppingCart("Chocolate" :: "Trousers" :: Nil, 10.5) :: Nil))


}
