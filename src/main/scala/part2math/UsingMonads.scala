package part2math

object UsingMonads {

  import cats.Monad
  import cats.instances.list._

  val monadList = Monad[List] // list monad type class instance
  val aSimpleList = monadList.pure(2) // List(2)
  val mapped = monadList.flatMap(aSimpleList)(elem => List(elem - 1, elem, elem + 1)) // applicable to Option, Try, Future etc..

  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[A] = Either[String, A]
  type ErrorOr[A]   = Either[Throwable, A]

  import cats.instances.either._

  val loadingMonad = Monad[LoadingOr]
  val errorOrMonad = Monad[ErrorOr]

  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither) { int =>
    if (int >= 5) Left("Too much")
    else Right(int - 1)
  }

  // imaginary online store

  final case class OrderStatus(orderId: Long, status: String)

  // Right(orderStatus) or Left(String error message)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId = orderId, status = "ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, Netherlands")

  val orderId = 457L

  val orderLocation: LoadingOr[String] = loadingMonad.flatMap(getOrderStatus(orderId)) { stat =>
    trackLocation(stat)
  }

  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLoc: LoadingOr[String] = for {
    status   <- getOrderStatus(orderId)
    location <- trackLocation(status)
  } yield location

  // exercise: service api layer for web app
  case class Connection(host: String, port: String)
  val config: Map[String, String] = Map(
    "host" -> "localhost",
    "port" -> "8080"
  )

  trait HttpService[F[_]] {
    def getConnection(cfg: Map[String, String]): F[Connection]
    def issueRequest(connection: Connection, payload: String): F[String]
  }

  // Requirements
  // if the host and port are found in the config map then return a M containing a connection with those
  // or else method will fail according to the logic of the type F
  // for Try it will return Failure, Option -> None, Future -> Failed, Either -> Left etc

  // the issueRequest method returns a F containing the string: "request" (payload) has been accepted", if the
  // payload is less than

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = for {
      h <- cfg.get("host")
      p <- cfg.get("port")
    } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length >= 20) None
      else Some(s"Request $payload has been accepted.")

  }

  object AggressiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (!cfg.contains("host") || !cfg.contains("port")) Left(new RuntimeException("Connection could not be established"))
      else Right(Connection(cfg("host"), cfg("port")))

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("Request size too big."))
      else Right(s"Request $payload has been accepted.")
  }

  import cats.syntax.flatMap._ // brings in for comprehensions
  import cats.syntax.functor._ // brings in map
  def getResponse[F[_]: Monad](service: HttpService[F], payload: String): F[String] = for {
    conn <- service.getConnection(config)
    resp <- service.issueRequest(conn, payload)
  } yield resp

  def main(args: Array[String]): Unit = {
    val responseOption = OptionHttpService.getConnection(config).flatMap(conn => OptionHttpService.issueRequest(conn, "OptionHttpService"))

    val forCompr = for {
      conn <- OptionHttpService.getConnection(config)
      resp <- OptionHttpService.issueRequest(conn, "OptionHttpService")
    } yield resp

    println(responseOption)
    println(forCompr)

    println {
      for {
        conn <- AggressiveHttpService.getConnection(config)
        resp <- AggressiveHttpService.issueRequest(conn, "Payload")
      } yield resp
    }

    println {
      getResponse(AggressiveHttpService, "payload")
    }

    println {

    }

  }

}
