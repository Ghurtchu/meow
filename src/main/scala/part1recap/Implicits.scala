package part1recap

object Implicits {

  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  val impersonableString = new ImpersonableString("Peter")
  val implicitGreeting = impersonableString.greet

  val greetingImplicit = "Peter".greet // new ImpersonableString("Peter").greet

  import scala.concurrent.duration._

  val oneSec = 1.second

  def increment(x: Int)(implicit by: Int): Int = x + by

  def main(args: Array[String]): Unit = {

    implicit val five: Int = 5
    println(increment(10))

  }

  // more complex example

  trait JsonSerializer[A] {
    def toJson(value: A): String
  }

  def listToJson[A](list: List[A])(implicit serializer: JsonSerializer[A]): String =
    list.map { v =>
      serializer.toJson(v)
    }.mkString("[", ",", "]")

  implicit val personSerializer = new JsonSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
        |{
        |"name": "${value.name}"
        |}
        |""".stripMargin
  }

  val personJson = listToJson(List(
    Person("Alice"),
    Person("Bob")
  ))

  println(personJson)

  // implicit methods
  implicit def oneArgCaseClassSerializer[A <: Product]: JsonSerializer[A] = new JsonSerializer[A] {
    override def toJson(value: A): String =
      s"""
         |  {
         |     "${value.productElementName(0)}": "${value.productElement(0)}"
         |  }
         |""".stripMargin
  }

  final case class Cat(catName: String)

  val cat = Cat("Tommy")

  println(oneArgCaseClassSerializer[Cat].toJson(cat))

  println(listToJson[Cat](List(Cat("Nikko"), Cat("Grikko"))))

  // background val catsToJson = listToJson(List(Cat()....))(oneArgCaseClassSerializer[Cat])
  // implicit methods are used to prove the existence of a type

  // implicit defs can also be used for type conversions


}
