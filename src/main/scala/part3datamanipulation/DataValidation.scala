package part3datamanipulation

object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // kinda equivalent to Right(42)
  val invalidValue: Validated[String, Boolean] = Validated.invalid("Error") // kinda equivalent to Left("Error")

  val aTest: Validated[String, Int] = Validated.cond(42 > 40, 99, "Invalid expression")

  // TODO: user Either for following example

  def main(args: Array[String]): Unit = {

  }

}
