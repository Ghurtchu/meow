package part1recap

object CatsIntro {


  // part 1 - import type class itself
  import cats.Eq

  // part 2 - import type class instances for types we need (Int, String etc..)
  import cats.instances.int._ // all the type class instances defined for int

  // part 3 - use the type class API
  val intEquality: Eq[Int] = Eq[Int] // summoning of instance by giving the type to apply method
  val aTypeSafeComparison: Boolean = intEquality.eqv(5, 10) // test equality between Int instances
  // val anUnsafeComparison = intEquality.eqv(2, "10") - does not compile

  // part 4 - use extension methods if applicable
  import cats.syntax.eq._ // ===
  println(2 === 3)

  val eqComparison = 2 === 3
  val notEqComparison = 2 =!= 3

  // how to work with composite types such as Lists? Options? Trys?

  // part 5 - extending the type class operations to composite types

  import cats.instances.list._ // bring Eq[List[Int]] in scope
  val aListComparison = List(2) === List(3)

  // part 6 - what about type class instances for our business domain entities?
  // create type class instance for a custom type
  final case class ToyCar(model: String, price: BigDecimal)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (c1, c2) =>
    c1.price == c2.price
  }

  println((ToyCar("m1", BigDecimal(30.5)) == ToyCar("Lambo", BigDecimal(32.4))))


  def main(args: Array[String]): Unit = {

  }

}
