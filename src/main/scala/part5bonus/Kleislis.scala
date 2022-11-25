package part5bonus

import cats.data

object Kleislis {

  // Kleisli = A => F[B] = Int => Option[String]

  val func1: Int => Option[String] = n => if (n % 2 == 0) Some("x is even") else None
  val func2: Int => Option[Int] = n => Some(n * 3)
  // val func3 = func2 andThen func1

  // composing this is easier
  val plainFunc1: Int => String = n => if (n % 2 == 0) "x is even" else ""
  val plainFunc2: Int => Int = n => n * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  import cats.data.Kleisli // Heinrich Kleisli

  val func1Kleisli: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2Kleisli: Kleisli[Option, Int, Int] = Kleisli(func2)

  import cats.instances.option._ // implicit FlatMap of option

  val func3Kleisli: Kleisli[Option, Int, String] = func2Kleisli andThen func1Kleisli

  // convenience
  val multiply = func2Kleisli.map(_ * 2)
  val chain = func2Kleisli.flatMap(n => func1Kleisli.map(b => s"${n + b}"))

  import cats.Id // Identity = Id[A] = A

  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // same as Reader[A, B]

  val times2 = Kleisli[Id, Int, Int](n => n * 2)
  val plus4  = Kleisli[Id, Int, Int](n => n + 4)

  val composed = times2.flatMap(t2 => plus4.map(p4 => p4 + t2))
  val composedKleisli = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  // sounds like DI => Reader.



  def main(args: Array[String]): Unit = {

  }
}
