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

}
