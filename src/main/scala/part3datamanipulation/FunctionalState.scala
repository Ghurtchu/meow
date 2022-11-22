package part3datamanipulation

object FunctionalState {

  // from state you get state with (new state, answer (desirable value))

  import cats.data.State
  type MyState[S, A] = S => (S, A)

  val countAntSay: State[Int, String] = State { currentCount =>
    (currentCount + 1, s"Counted $currentCount")
  }

  val (eleven, countedTen) = countAntSay.run(10).value
  // state = "iterative computation"

  object bad_scala_code {
    var a = 10
    a += 1
    val firstComputation = s"1 added to 10, obtained $a"
    a *= 5
    val secondComputation = s"multiplied by 5, obtained $a"
  }

  // pure FP with states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10, obtained $s"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied by 5, obtained $s"))

  val composite: State[Int, (String, String)] = firstTransformation.flatMap(firstRes => secondTransformation.map(secondRes => (firstRes, secondRes)))

  val composite2: State[Int, (String, String)] = for {
    f1 <- firstTransformation
    f2 <- secondTransformation
  } yield (f1, f2)

  val compositeAcc: State[Int, List[String]] = for {
    f1 <- firstTransformation
    f2 <- secondTransformation
  } yield f1 :: f2 :: Nil

  object functions {

    val f1: Int => (Int, String) = n => (n + 1, "added 1")
    val f2: Int => (Int, String) = n => (n * 10, "multiplied by 10")

    val compositeFunc =
      f1.andThen {
        case (num, msg) => (f2(num), msg concat msg)
      } // does not scale well

  }

  object todo_online_store {

    final case class ShoppingCart(items: List[String], total: Double)

    def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
      State(s => (ShoppingCart(item :: s.items, price + s.total), price + s.total))

    val state = addToCart("a", 10.0)
      .flatMap(_ => addToCart("b", 20.0))
      .flatMap(_ => addToCart("c", 30.0))
      .run(ShoppingCart(Nil, 0.0))
      .value
  }

  object mental_gymnastic_exercises {

    def inspect[A, B](f: A => B): State[A, B] = State[A, B](a => (a, f(a)))

    def get[A]: State[A, A] = State[A, A](a => (a, a))

    def set[A](value: A): State[A, Unit] = State[A, Unit](a => (a, ()))

    def modify[A](f: A => A): State[A, Unit] = State[A, Unit](a => (f(a), ()))

    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a + 10)
      b <- get[Int]
      _ <- modify[Int](_ + 43)
      c <- inspect[Int, Int](_ * 2)
    } yield (a, b, c)

  }

  def main(args: Array[String]): Unit = {

    println(composite.run(10).value)
    println(compositeAcc.run(50).value)
    println(todo_online_store.state)
    println(mental_gymnastic_exercises.program.run(10).value)

  }


}
