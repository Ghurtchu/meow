package part2math

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.util.Try

object Monads {

  // lists

  val numbers = 1 :: 2 :: 3 :: Nil
  val chars   = 'a' :: 'b' :: 'c' :: Nil

  // all combinations of (int, char)
  val combination: List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))

  val combinationForComp: List[(Int, Char)] =
    for {
      n <- numbers
      c <- chars
    } yield (n, c)

  // options
  val nOpt = Option(2)
  val cOpt = Option('d')
  val optComb: Option[(Int, Char)] = nOpt.flatMap(n => cOpt.map(c => (n, c)))
  val optCombForComp: Option[(Int, Char)] =
    for {
      n <- nOpt
      c <- cOpt
    } yield (n, c)

  import scala.concurrent.Future
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val numFut = Future(42)
  val charFut = Future('Z')

  val comb = numFut.flatMap(n => charFut.map(c => (n, c)))
  val combForComp =
    for {
      n <- numFut
      c <- charFut
    } yield (n, c)

  /**
   * Pattern
   * - wrapping a value into a M value
   * - the flatMap mechanism
   */

  trait MyMonad[F[_]] {
    def pure[A](value: A): F[A] // wrap
    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(f andThen pure)
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] // chaining computations which require result from previous ones
  }

  // Cats monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad = Monad[Option] // summoner which summons implicit instance from scope
  val anOption = optionMonad.pure[Int](5)
  val flatMapped = optionMonad.flatMap[Int, String](anOption)(n => Some(n.toString))

  import cats.instances.list._

  val listMonad: Monad[List] = Monad[List]
  val aList = listMonad.pure[Int](5)
  val flatMappedList = listMonad.flatMap[Int, String](aList)(_ => "xD" :: Nil)

  import cats.instances.future._
  import cats.instances.try_._

  // specialized API, combine two generic values wrapped by container
  def pairs[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    Monad[F].flatMap(fa)(a => Monad[F].map(fb)(b => (a, b)))

  pairs[Option, Int, String](Option(5), Option("5")) // Option((5, "5))
  pairs[Future, Boolean, Boolean](Future(true), Future(false)) // Future((true, false))
  pairs[Try, Char, Short](Try('c'), Try(0)) // Try(('c', 0))

  // extension methods - methods defined on F[_]

  import cats.syntax.applicative._ // pure is defined on applicative
  val opt = 1.pure[Option] // will use implicit Monad[Option]
  val lst = 1.pure[List]

  import cats.syntax.flatMap._ // flatMap is here for type constructors (Option, List, Future, Try, Either etc...)
  import cats.syntax.option._

  val oneOptionTransformed = opt.flatMap(x => (x + 1).some)
  val oneOptionTransformed2 = opt.flatMap(x => (x + 1).pure[Option])

  import cats.syntax.functor._ // map extension method for type constructors (Option, List, Future, Try, Either etc..)
  val d = 1.pure[Option].map(_ + 2)
  val comp = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // implement shorted version of getPairs using for-compr
  def pairsWithForExpression[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)



}
