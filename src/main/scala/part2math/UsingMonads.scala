package part2math

import scala.util.Try

object UsingMonads {

  import cats.Monad
  import cats.instances.list._ // import list instances for Monad, Functor, and many others

  val listMonad: Monad[List] = Monad[List] // fetch the implicit Monad[List]
  val aList: List[Int] = listMonad.pure(2)
  val extendedList: List[Int] = listMonad.flatMap(aList)(n => List(n - 1, n, n + 2))
  // fundamental methods are: pure, flatMap

  // applicable to Option, Try, Future, Either, List, Vector, etc..

  val aManualEither: Either[String, Int] = Right(77)

  type LoadingOr[A] = Either[String, A] // String -> some sort of loading error message
  type ErrorOr[A]   = Either[Throwable, A]

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr] // fetch instance
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither) { a =>
    if (scala.util.Random.nextBoolean) Right(a.toString)
    else Left("Boom")
  }

  // imaginary online store
  final case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available, refreshing data...")
    else Right("Amsterdam, Netherlands")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId)) { status =>
    trackLocation(status)
  }

  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val result: LoadingOr[String] = for {
    status <- getOrderStatus(orderId) // LoadingOr[OrderStatus]
    loc    <- trackLocation(status)   // LoadingOr[String]
  } yield loc

  final case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[F[_]] {
    def getConnection(config: Map[String, String]): F[Connection] = ???
    def issueRequest(connection: Connection, payload: String): F[String] = ???
  }

  def sendRequest[F[_]: Monad](configuration: Map[String, String], httpService: HttpService[F], message: String): Unit = for {
    conn <- httpService.getConnection(configuration)
    _    <- httpService.issueRequest(conn, message)
  } yield ()

  class HttpServiceImpl extends HttpService[Try] {

    override def getConnection(config: Map[String, String]): Try[Connection] = {
      if (config.isDefinedAt("host") && config.isDefinedAt("port")) scala.util.Success(Connection(config("host"), config("port")))
      else scala.util.Failure(new IllegalStateException("both host and port are required"))
    }

    override def issueRequest(connection: Connection, payload: String): Try[String] = {
      println(s"sending msg: $payload")
      scala.util.Success("Request has been sent successfully")
    }

  }

  class HttpServiceOption extends HttpService[Option] {
    override def getConnection(config: Map[String, String]): Option[Connection] = {
      if (config.isDefinedAt("host") && config.isDefinedAt("port")) Some(Connection(config("host"), config("port")))
      else None
    }

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      println(s"sending msg: $payload")
      Some("Request has been sent successfully")
    }
  }

  class HttpServiceLoadingOr extends HttpService[LoadingOr] {
    override def getConnection(config: Map[String, String]): LoadingOr[Connection] = {
      if (config.isDefinedAt("host") && config.isDefinedAt("port")) Right(Connection(config("host"), config("port")))
      else Left("Something went wrong cuz we lack host and port")
    }
    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] = {
      println(s"sending msg: $payload")
      Right("Request has been sent successfully")
    }
  }

  val serviceTry       = new HttpServiceImpl
  val serviceOption    = new HttpServiceOption
  val serviceLoadingOr = new HttpServiceLoadingOr

  import cats.instances.try_._
  import cats.instances.option._

  def main(args: Array[String]): Unit = {

    sendRequest[Try](config, serviceTry, "Hello, baby!")
    sendRequest[Option](config, serviceOption, "Hey hey")
    sendRequest[LoadingOr](config, serviceLoadingOr, "Hello, sweetheart")

  }

}
