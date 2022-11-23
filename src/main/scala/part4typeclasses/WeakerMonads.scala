package part4typeclasses

object WeakerMonads {

  import cats.FlatMap
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.instances.list._

  def getPairs[F[_]: FlatMap, A, B](ns: F[Int], chs: F[Char]): F[(Int, Char)] =
    for {
      n <- ns
      c <- chs
    } yield (n, c)

  def main(args: Array[String]): Unit = {
    println(getPairs(1 :: 2 :: 3 :: Nil, 'a' :: 'b' :: 'c' :: Nil))
  }

}
