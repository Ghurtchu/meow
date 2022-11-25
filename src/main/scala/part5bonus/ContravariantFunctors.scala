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

  }

  def format[A: Format](value: A): String = Format[A].format(value)

  def main(args: Array[String]): Unit = {
    println(format(5))
    println(format("Nothing weird so far"))
    println(format(true))
  }

}
