package part2math

object Evaluation {

  // Eval
  // - eager
  // lazily (recomputed every time)
  // lazily (meomoized)

  import cats.Eval

  val instant: Eval[Int] = {
    println("Computing now")

    Eval.now[Int](42) // a simple wrapper
  }

  val onRequest: Eval[String] = {
    println("Computing every time like scala def")

    Eval.always(scala.util.Random.nextString(10))
  }

  val lazyMemoized: Eval[Boolean] = {
    println("Computing on request but only once")

    Eval.later(true)
  }

  val composed: Eval[List[Any]] = for {
    i <- instant
    o <- onRequest
    l <- lazyMemoized
  } yield i :: o :: l :: Nil

  val tutorial = Eval.always {
    println("Step 1")
    "Put the guitar on your lap"
  }.map { s =>
    println("Step 2")
    s"$s then put your left hand on the neck"
  }.memoize.map { steps => // memoize up to this point so step 1 and step 2 will not be recomputed
    println("Step 3")
    s"$steps and then play that got damn guitar" // it will be recomputed
  }

  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  def main(args: Array[String]): Unit = {
    instant.value // like scala val

    println(onRequest.value) // like scala def
    println(onRequest.value) // like scala def

    println(lazyMemoized.value) // like scala lazy val
    println(lazyMemoized.value) // like scala lazy val

    println(composed.value)

    println(tutorial.value)
    println(tutorial.value)
  }

}
