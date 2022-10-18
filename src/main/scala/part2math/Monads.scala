package part2math

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import cats.instances.try_._

object Monads {


  // lists
  val numbers = 1 :: 2 :: 3 :: Nil
  val chars   = 'a' :: 'b' :: 'c' :: Nil

  val combination = for {
    n <- numbers
    c <- chars
  } yield (n, c)

  val maybeNum = Option(5)
  val maubeChar = Option('5')

  val combination2: Option[(Int, Char)] = for {
    n <- maybeNum
    c <- maubeChar
  } yield (n, c)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val numberFuture = Future(42)
  val charFuture = Future('c')

  val combinedFuture: Future[(Int, Char)] = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /**
   * Pattern
   - wrapping a value into a M value
   - the flatMap mechanism

   MONADS
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A] // lift
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  // Cats monad
  import cats.Monad
  import cats.instances.option._

  val optionMonad = Monad[Option]
  val lifted: Option[Int] = optionMonad.pure(5) // Option(4)
  val aTransformedOption: Option[Int] = optionMonad.flatMap(lifted)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(l => List(l, l+1, l+2))

  // future
  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(5) // lift into future
  val aTransformedFuture = futureMonad.flatMap(aFuture)(fut => Future(fut + 5))

  def main(args: Array[String]): Unit = {
    println(aTransformedList)
    println(aTransformedFuture)
    println {
      pairs(1 :: 2 :: Nil, 'a' :: 'b' :: Nil)
    }
  }

  // specialized API
  def getPairs(ints: List[Int], chars: List[Char]): List[(Int, Char)] = for {
    n <- ints
    c <- chars
  } yield (n, c)


  // you can't generalize more than that
  def pairs[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    Monad[F].flatMap(fa)(a => Monad[F].map(fb)(b => (a, b)))

  val listTuples: List[(Int, Char)] =
    pairs(1 :: 2 :: Nil, 'a' :: 'b' :: Nil)
  val futureTuple: Future[(Int, String)] =
    pairs(Future(42), Future("past"))
  val tryTuple: Try[(Int, Boolean)] =
    pairs(Try(42 / 0), Try(10 % 2 == 0))
  val optionTuple: Option[(BigDecimal, BigInt)] =
    pairs(Option(BigDecimal(1L)), Option(BigInt(2)))

}
