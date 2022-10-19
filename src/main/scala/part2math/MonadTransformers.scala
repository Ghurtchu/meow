package part2math

import cats.data.OptionT // higher kinded type
import cats.instances.list._ // fetch implicit OptionT[List]

object MonadTransformers {

  // option transformer
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // List[Option[Int]]
  val listOfOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('1'), Option('2'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    num  <- listOfOptions
  } yield (num, char)

  // either transformer
  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] =
    EitherT(
      List(
        Left("something wrong"),
        Right(43),
        Right(300)
      )
    )

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  val futureOfEither: EitherT[Future, String, Int] =
    EitherT(
      Future(
        Right(300)
      )
    )

  def main(args: Array[String]): Unit = {

  }

}
