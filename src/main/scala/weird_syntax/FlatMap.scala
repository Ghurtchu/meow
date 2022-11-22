package weird_syntax

import cats.syntax.flatMap._
import cats.instances.option._

object FlatMap extends scala.App {

  val withoutSyntax = Option(1).flatMap(_ => Option(2))
  val withSyntax    = Option(1) >> Option(2)

  println(withoutSyntax)
  println(withSyntax)

}
