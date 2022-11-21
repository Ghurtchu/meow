package part2math

import cats.data.EitherT

object MonadTransformers {

  object without_monad_transformers_1 {
    def sumAllOptions(options: List[Option[Int]]): Int = options.map {
      case Some(value) => value
      case None => 0
    }.sum

    def sumAllOptions2(os: List[Option[Int]]): Int = os.filter(_.isDefined).map(_.get).sum

    def sumAllOpts(options: List[Option[Int]]): Int = (for {
      opt <- options
    } yield opt.fold(0)(identity)).sum
  }

  import cats.data.OptionT // OptionTransformer
  import cats.instances.list._ // fetch an implicit OptionT[List]

  object monad_transformers {

    val eitherT: EitherT[List, Char, Boolean] = EitherT(Left('a') :: Left('b') :: Left('c') :: Right(true) :: Right(false) :: Nil)
    val optionT: OptionT[List, Int] = OptionT(Some(1) :: Some(2) :: None :: None :: Some(3) :: Nil)

    val data: OptionT[List, (Boolean, Int)] = for {
      bool <- eitherT.toOption
      int  <- optionT
    } yield (bool, int)

  }

  object monad_transformers_2 {

    // list of tries
    import cats.data.OptionT
    val l1: OptionT[List, Int] = OptionT(Option(1) :: Option(2) :: Nil)
    val l2: OptionT[List, Int] = OptionT(Option(10) :: Option(20) :: Nil)

    val res: OptionT[List, (Int, Int)] = for {
      a <- l1
      b <- l2
    } yield (a, b)

    // without monad transformers we'd write
    val list1: List[Option[Int]] = Some(1) :: Some(2) :: Nil
    val list2: List[Option[Int]] = Some(10) :: Some(20) :: Nil

    val res2 = for {
      opt1 <- list1
      opt2 <- list2
    } yield (opt1, opt2) match {
      case (Some(a), Some(b)) => (a, b)
      case _                  => None
    }


  }

  // OptionT[List, Int] => List[Option[Int]] = middle -> left -> right
  val intOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val charOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), None))

  // List[Either[Int, String]] => EitherT
  val listEithers: EitherT[List, Int, String] = EitherT(List(Left(2), Left(3), Right("Yeah!"), Right("No!")))

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  // Future[Option[Boolean]] = OptionT[Future, Boolean]
  val futureOfOptionOFBoolean: OptionT[Future, Boolean] = OptionT {
    Future(Option(true))
  }


  // let's say you wanna couple (Int, Char)
  // Since they are wrapped by Option it will be clunky as hell

  val tuples: OptionT[List, (Int, Char)] = for {
    char <- charOptions
    num  <- intOptions
  } yield (num, char)

  val values: Seq[Option[(Int, Char)]] = tuples.value

  // convenience API which let's you to forget unwrapping your inner monads(option) all the time
  // and provides flatMap and map methods
  // when you have wrapper monad(list) over your own monad(option)

  object without_monad_transformers_2 {

    val ints = Option(1) :: Option(2) :: Nil
    val chars = Option('a') :: Option('b') :: None :: Nil

    val result: Seq[Option[(Int, Char)]] = for {
      optC <- chars
      optI <- ints
    } yield (optC, optI) match {
      case (Some(c), Some(i)) => Option((i, c))
      case _ => None
    }

  }

  val eithers: EitherT[List, String, Int] = EitherT(
    List(
      Right(5),
      Right(10),
      Left("Wrong"),
      Left("Wrong"),
      Left("Wrong"),
      Right(100)
    )
  )


  def main(args: Array[String]): Unit = {
    println(monad_transformers_2.res)
    println(monad_transformers_2.res2)
  }

}
