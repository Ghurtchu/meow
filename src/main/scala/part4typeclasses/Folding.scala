package part4typeclasses

import cats.Monoid


object Folding {

  object list_exercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = ???
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = ???
    def filter[A](list: List[A])(f: A => Boolean): List[A] = ???
    def combine[A: Monoid](list: List[A]): A = ???
  }

}
