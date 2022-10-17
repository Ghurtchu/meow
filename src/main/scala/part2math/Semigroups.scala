package part2math

// semigroups combine elements of the same type
object Semigroups {

  import cats.Semigroup
  import cats.instances.int._
  import cats.instances.string._
  import cats.instances.option._ // compiler will produce an implicit Semigroup[Option[Int]]

  import cats.instances.either._

  val naturalIntSemigroup: Semigroup[Int] = Semigroup[Int] // type class instance
  val intCombination: Int = naturalIntSemigroup.combine(2, 46) // addition

  val naturalStringSemigroup: Semigroup[String] = Semigroup[String] // type class instance
  val stringCombination: String = naturalStringSemigroup.combine("I love", "Scala")

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general API based on types/generics
  def reduceAny[A: Semigroup](list: List[A]): A = list.reduce(Semigroup[A].combine)

  final case class Expense(id: Long, amount: Double)

  // create of instance of Semigroup[Expense]

  implicit val expenseSemigroup: Semigroup[Expense] = (x: Expense, y: Expense) => Expense(Math.max(x.id, y.id), x.amount + y.amount)

  // extension methods - |+|
  import cats.syntax.semigroup._

  val combined: String = "This will" |+| "be combined"

  def main(args: Array[String]): Unit = {

    println(intCombination)
    println(stringCombination)

    println {
      reduceAny(1 :: 2 :: 3 :: Nil)
    }

    println {
      reduceAny("1" :: "2" :: "3" :: Nil)
    }

    println {
      reduceAny(Option(1) :: Option(2) :: None :: Nil)
    }

    println {
      reduceAny(Expense(1, 100) :: Expense(2, 200) :: Expense(3, 300) :: Nil)
    }

    println {
      reduceAny3(Expense(1, 100) :: Expense(2, 200) :: Nil)
    }

  }

  // implement reduceAny2[A]
  def reduceAny2[A](list: List[A])(implicit semigroup: Semigroup[A]): A = list.reduce(_ |+| _)
  def reduceAny3[A: Semigroup](list: List[A]): A = list.reduce(_ |+| _)


}
