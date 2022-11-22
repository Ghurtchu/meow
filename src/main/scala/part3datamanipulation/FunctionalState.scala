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


  def main(args: Array[String]): Unit = {

    println(composite.run(10).value)
    println(compositeAcc.run(50).value)

  }


}
