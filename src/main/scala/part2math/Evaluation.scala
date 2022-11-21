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

  def main(args: Array[String]): Unit = {
    instant.value // like scala val

    println(onRequest.value) // like scala def
    println(onRequest.value) // like scala def

    println(lazyMemoized.value) // like scala lazy val
    println(lazyMemoized.value) // like scala lazy val

    println(composed.value)

  }

}
