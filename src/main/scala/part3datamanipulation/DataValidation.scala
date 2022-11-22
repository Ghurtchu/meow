package part3datamanipulation

object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // kinda equivalent to Right(42)
  val invalidValue: Validated[String, Boolean] = Validated.invalid("Error") // kinda equivalent to Left("Error")

  val aTest: Validated[String, Int] = Validated.cond(42 > 40, 99, "Invalid expression")

  // TODO: user Either for following example

  /**
   * - must be positive
   * - less than 100
   * - must be even
   * - must not equal to 50
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    val isPositive: List[String] = if (n > 0) Nil else List("It is non-positive")
    val isLessThan100: List[String] = if (n < 100) Nil else List("Is 100 or more")
    val isEven: List[String] = if (n % 2 == 0) Nil else List("Is odd")
    val isNotEqualTo50: List[String] = if (n != 50) Nil else List("it's 50")

    if (isPositive.nonEmpty || isLessThan100.nonEmpty || isEven.nonEmpty || isNotEqualTo50.nonEmpty) Left(isPositive ::: isLessThan100 ::: isEven ::: isNotEqualTo50)
    else Right(n)
  }


  def main(args: Array[String]): Unit = {
    println(testNumber(4))
    println(testNumber(5))

  }

}
