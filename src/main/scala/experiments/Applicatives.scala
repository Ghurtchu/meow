package experiments

object Applicatives {

  // Applicative = Functor + pure method
  import cats.Applicative
  import cats.syntax.applicative._

  def main(args: Array[String]): Unit = {

    import cats.instances.list._
    val listApplicative = Applicative[List] // higher kinded type like functor, monad etc

    val lst1: List[Int] = listApplicative.pure[Int](10)
    val lst2: List[Int] = 10.pure[List]

    assert(lst1 == lst2)

    import cats.instances.option._
    implicit val optionApplicative: Applicative[Option] = Applicative[Option]

    val opt1: Option[String] = optionApplicative.pure[String]("Hello")
    val opt2: Option[Int]    = optionApplicative.pure[Int](42)

    // Monads extend Applicatives
    // Applicatives extend Functors

    import cats.data.Validated
    type ErrorOrs[A] = Validated[List[String], A]

    val aValidVal = Validated valid 40
    val mapped    = aValidVal.map(_ * 50)
    val applic    = Applicative[ErrorOrs]

    import cats.syntax.applicative._
    import cats.syntax.functor._

    object wow_i_am_amazed {
      val f: Int => String = _.toString
      val a: Option[Int => String] = Option(f)
    }

  }

}
