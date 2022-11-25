package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent._

object Traversing {

  // higher level API for doing iteration

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors()))

  val servers: List[String] = "server-ci.rockthejvm.com" :: "server-staging.rockthejvm.com" :: "prod.rockthejvm.com" :: Nil

  def getBandwidth(hostName: String): Future[Int] = Future(hostName.length * 80) // load capacity of the server

  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (acc, hostName) =>
    val bandFut: Future[Int] = getBandwidth(hostName)
    for {
      bandWidths <- acc
      band       <- bandFut
    } yield bandWidths :+ band
  }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse[String, Int, List](servers)(getBandwidth) // for each string call method
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth)) // List[Future[_]] => Future[List[_]]

  import cats.Monad
  import cats.syntax.applicative._ // for pure method pure is like a => f[a]
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def listTraverse[F[_]: Monad, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wrapperAcc, elem) =>
      val wrapped: F[B]= f(elem)
      for {
        acc  <- wrapperAcc
        elem <- wrapped
      } yield acc :+ elem
    }


  def main(args: Array[String]): Unit = {

  }

}
