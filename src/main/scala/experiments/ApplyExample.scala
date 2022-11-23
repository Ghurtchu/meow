package experiments

import cats.{Functor, Semigroupal}

object ApplyExample {

  import cats.Apply

  import cats.instances.option._

  val applyOption: Apply[Option] = Apply[Option]

  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2))

  import cats.syntax.apply._ // extension methods for apply

  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple  = tupleOfOptions.tupled // unwraps all this options Option(1, 2, 3)

  def main(args: Array[String]): Unit = {
    println(optionOfTuple)
    println((None, Some(1), None).tupled)
    val sumOption = tupleOfOptions.mapN(_ + _ + _) // Some(6)
    println(sumOption)
  }


}
