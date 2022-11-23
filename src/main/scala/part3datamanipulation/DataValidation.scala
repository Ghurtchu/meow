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

    if (isPositive.nonEmpty || isLessThan100.nonEmpty || isEven.nonEmpty || isNotEqualTo50.nonEmpty) Left(isPositive ++ isLessThan100 ++ isEven ++ isNotEqualTo50)
    else Right(n)
  }

  import cats.Semigroup
  import cats.instances.list._

  implicit val intSemigroup: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] = {
    Validated.cond(n > 0, n, List("Number must be even"))
      .combine(Validated.cond(n < 100, n, List("Number must be non-negative")))
      .combine(Validated.cond(n % 2 == 0, n, List("Is odd")))
      .combine(Validated.cond(n != 50, n, List("it's 50")))
  }

  aValidValue
    .andThen(n => Validated.valid(n + 1))

  aValidValue
    .ensure(List("Something went wrong")) { n =>
      n % 2 == 0
    }

  aValidValue.map( _ + 1)
  aValidValue.leftMap(err => err concat "!")
  aValidValue.bimap(err => err concat "!", _ + 10)

  val valid = Validated.fromOption(Some(10), 10) // fromTry, fromEither
  valid.toOption
  valid.toEither // etc

  def main(args: Array[String]): Unit = {
    println(testNumber(4))
    println(testNumber(5))

    println(validateNumber(98))
  }

  object form_validation {

    type FormValidation[A] = Validated[List[String], A]

    // conditions(fields: [name, email, password])
    // - name
    // - email
    // - password
    // rules are:
    // - name, email and password must be specified
    // - name must not be blank
    // - email must contain `@`
    // - password must have >= 10 characters

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form get fieldName, List(s"Field $fieldName must be specified"))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.length > 0, value, List(s"field $fieldName must not be blank"))

    def emailCheck(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List(s"Email must contain `@`"))

    def ensurePassword(pass: String): FormValidation[String] =
      Validated.cond(pass.length >= 10, pass, List(s"Password must be lengthyyyyy"))

    def validateForm(form: Map[String, String]): FormValidation[String] = {

      import cats.instances.string._ // implicit instance of Semigroup[String] which is just concatenation

      getValue(form, "Name")
        .andThen(name => nonBlank(name, "Name"))
        .combine(getValue(form, "Email").andThen(emailCheck))
        .combine(getValue(form, "Password").andThen(ensurePassword))
        .map(_ => "All requirements pass")

    }


  }

}
