package part5bonus

object ContravariantFunctors {

  trait Format[A] {
    def format(value: A): String
  }

  object Format {

    def apply[A](implicit format: Format[A]): Format[A] = format

    implicit object strFormat extends Format[String] {
      override def format(value: String): String = s""""$value""""
    }

    implicit object intFormat extends Format[Int] {
      override def format(value: Int): String = value.toString
    }

    implicit object boolFormat extends Format[Boolean] {
      override def format(value: Boolean): String = if (value) "Y" else "N"
    }

    // problem: given Format[MyType] can we also have Format[Option[MyType]]?
    implicit def getOptionFormat[T: Format]: Format[Option[T]] = new Format[Option[T]] {
      override def format(value: Option[T]): String = Format[T].format(value.get)
    }

    def contramap[A, B](f: A => B)(implicit form: Format[B]): Format[A] = new Format[A] {
      override def format(value: A): String = form.format(f(value))
    }

  }

  def format[A: Format](value: A): String = Format[A].format(value)

  def main(args: Array[String]): Unit = {
    println(format(5))
    println(format("Nothing weird so far"))
    println(format(true))


    import cats.Contravariant
    import cats.Show // type class for turning things into toString
    import cats.instances.int._ // implicit Show[Int]

    val showInt = Show[Int]
    val showOpt: Show[Option[Int]] = Contravariant[Show].contramap(showInt)(_.get)


  }

}
