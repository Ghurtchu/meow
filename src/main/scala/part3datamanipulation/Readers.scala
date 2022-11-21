package part3datamanipulation

object Readers {

  /**
   * layers:
   * - config file
   * - DB layer (db credentials in config file)
   * - HTTP layer (port, host and other things in config file)
   * - a business logic layer (may depend on config as well)
   * Goal: feed config to all these services
   */

  final case class Config(dbUserName: String, dbPassword: String, host: String, port: Int, nThreads: Int)

  final case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = {
      // run queries in db
      "Successful"
    }
    def getLastOrderId(userName: String): Long = 10
  }

  final case class HttpService(host: String, port: Int) {
    def start(): Unit = println(s"Server started, listening on port: $port")
  }

  final case class ExecutionContextService(nThreads: Int)

  val config: Config = Config("nika", "pass-pass", "localhost", 1234, 8)

  import cats.data.Reader

  val dbReader: Reader[Config, DbConnection] = Reader { cfg =>
    DbConnection(cfg.dbUserName, cfg.dbPassword)
  }

  val httpServiceReader: Reader[Config, HttpService] = Reader { cfg =>
    HttpService(cfg.host, cfg.port)
  }

  val ecReader: Reader[Config, ExecutionContextService] = Reader { cfg =>
    ExecutionContextService(cfg.nThreads)
  }

  val conn: DbConnection          = dbReader run config
  val http: HttpService           = httpServiceReader run config
  val ec: ExecutionContextService = ecReader run config

  val orderStatus: Reader[Config, String] = dbReader.map(_.getOrderStatus(5))

  val reader: Reader[Config, Long] = dbReader
    .map(_.getOrderStatus(10))
    .flatMap(lastOrderId => dbReader.map(_.getLastOrderId(lastOrderId)))


  val forExpression: Reader[Config, Long] = for {
    lastOrderId <- dbReader.map(_.getOrderStatus(10))
    status      <- dbReader.map(_.getLastOrderId(lastOrderId))
  } yield status

  object exercise {

    case class EmailService(emailReplyTo: String) {
      def sendEmail(address: String, contents: String): String =
        s"From $emailReplyTo to $address >>>>> $contents"
    }

    val emailServiceReader: Reader[Config, EmailService] = Reader(cfg => EmailService(cfg.dbUserName))

    def emailUser(username: String, userEmail: String) = {
      // fetch status of their last order
      // email them with the email service: "Your order status: $status"

      val a = for {
        lastOrderStatus <- dbReader.map(_.getLastOrderId(username))
        email <- emailServiceReader.map(_.sendEmail("awdwad", s"Hello, status: $lastOrderStatus"))
      } yield email

      import cats.Id

      val email: Id[String] = a.run(config)

    }

    // Reader reminds me of dependency injection!

  }


}
