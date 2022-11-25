package part4typeclasses

import cats.Monoid


object Folding {

  object list_exercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B])((elem, acc) => f(elem) :: acc)
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldRight(List.empty[B])((elem, acc) => f(elem) ++ acc)
    def filter[A](list: List[A])(f: A => Boolean): List[A] = ???
    def combine[A: Monoid](list: List[A]): A = list.foldLeft(Monoid[A].empty)(Monoid[A].combine)
  }

  def main(args: Array[String]): Unit = {

    import list_exercises._

    val data = 1 :: 2 :: 3 :: Nil
    assert(map(data)(_ * 2) == 2 :: 4 :: 6 :: Nil)

    assert(flatMap(data)(n => List(n * 2)) == 2 :: 4 :: 6 :: Nil)



  }

}
