package part2math

import cats.data.EitherT

import scala.concurrent.Future

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

    import cats.syntax.flatMap._
    import cats.syntax.functor._

    val eitherT: EitherT[List, Char, Boolean] = EitherT(Left('a') :: Left('b') :: Left('c') :: Right(true) :: Right(false) :: Nil)
    val optionT: OptionT[List, Int] = OptionT(Some(1) :: Some(2) :: None :: None :: Some(3) :: Nil)

    val data: OptionT[List, (Boolean, Int)] = for {
      bool <- eitherT.toOption
      int  <- optionT
    } yield (bool, int)

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
    Future(Some(true))
  }

  // Option[Either[String, String]] = EitherT[Option, String, String]
  val eitherInOption: EitherT[Option, String, String] = EitherT(Some(Left("What?!")))

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

  import cats.data.EitherT // EitherTransformer

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

  import scala.concurrent.ExecutionContext.Implicits.global

  val futureOfEither: EitherT[Future, String, Int] = EitherT(Future(Left("Wrong")))
  val futureOfEither2: EitherT[Future, String, Int] = EitherT(Future(Right(42)))

  val bandwidth = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[A] = EitherT[Future, String, A]

  // Int = bandwidth of server
  def getBandwidth(serverName: String): AsyncResponse[Int] = bandwidth.get(serverName) match {
    case Some(value) => EitherT(Future(Right(value)))
    case None        => EitherT(Future(Left(s"Server $serverName unreachable")))
  }

  import cats.instances.future._

  // TODO 1
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    band1 <- getBandwidth(s1) // Future[Either[String, Int]] -> Future(Right(42)) ->
    band2 <- getBandwidth(s2)
  } yield band1 + band2 > 250

  // TODO 2
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers can not cope with spike due to $reason")
      case Right(false) => Left(s"Servers can not cope")
      case Right(true)  => Right("Servers can cope with the spike !!!")
    }

  def main(args: Array[String]): Unit = {
    println(values)
  }

}
