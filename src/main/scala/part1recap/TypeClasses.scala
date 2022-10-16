package part1recap

import part1recap.TypeClasses.JsonSyntax.JsonOps

object TypeClasses {

  final case class Person(name: String, age: Int)

  // part 1 - type class definition
  trait JsonSerializer[A] {
    def toJson(value: A): String
  }

  // part 2 - create implicit type class instances
  implicit object StringSerializer extends JsonSerializer[String] {
    override def toJson(value: String): String = "\"$value\""
  }

  implicit object IntSerializer extends JsonSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JsonSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         |{ "name": ${value.name}, "age": ${value.age} }
         |""".stripMargin.trim
  }

  // part 3 - offer some API for serializing things to json
  def convertListToJson[A](list: List[A])(implicit serializer: JsonSerializer[A]): String = {
    list.map(serializer.toJson).mkString("[", ",", "]")
  }

  // part 4 - extending the existging types via extension methods

  object JsonSyntax {
    implicit class JsonOps[A](value: A)(implicit serializer: JsonSerializer[A]) {
      def toJson: String = serializer.toJson(value)
    }
  }


  def main(args: Array[String]): Unit = {
    println(convertListToJson(List(Person("Alice", 23), Person("Xavier", 34))))

    Person("Alice", 23).toJson
  }

}
