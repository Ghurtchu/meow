package part3datamanipulation

import scala.annotation.tailrec

object Readers {

  // initial configuration file defines the following layers:
  // DB layer
  // http layer
  // business logic layer

  final case class Config(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, replyTo: String)

  final case class DBConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = {
      // select statements here ...

      "Mock Response"
    }

    def getLastOrderId(userName: String): Long = 123456L

  }

  final case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started") // this would start the actual server
  }

  // bootstrap
  val config: Config = Config("daniel", "rockthejvm!", "localhost", 1234, 8, "daniel@rockthejvm.com")

  // Reader = data processing type from cats
  import cats.data.Reader
  val dbReader: Reader[Config, DBConnection] = Reader { config =>
    DBConnection(config.dbUsername, config.dbPassword)
  }
  val dbConn = dbReader.run(config)

  // Reader[I, O]
  val dainelsOrderStatusReader: Reader[Config, String] = dbReader.map { conn =>
    conn.getOrderStatus(100L)
  }

  val danielsOrderStatus = dainelsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    (for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      status      <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield status).run(config)
  }

  /*
  * Pattern
  *  1. create initial data structure
  *  2. create reader which specifies how that data structure will be manipulated later
  *  3. you can then map & flatMap the reader to produce derived information
  *  4. when u need the final piece of information we call run with initial data structure
  */

  // TODO
  final case class EmailService(emailReplyTo: String) {
    def send(address: String, contents: String): String = s"Sending email to $address with $contents"
  }


  // fetch the status of their last order
  // and then email them with EmailService
  // "Your oder has the status: $status"

  def emailUser(username: String, userEmail: String, replyTo: String): Unit = for {
      service <- Reader[Config, EmailService](conf => EmailService(conf.replyTo))
      orderId <- Reader[DBConnection, Long](conn => conn.getLastOrderId(username))
      status  <- Reader[DBConnection, String](conn => conn.getOrderStatus(orderId))
      _       <- Reader[String, Unit](msg => service.send(userEmail, s"Your order has status $status"))
    } yield ()

  def main(args: Array[String]): Unit = {

  }

}
