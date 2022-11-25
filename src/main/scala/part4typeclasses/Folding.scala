package part4typeclasses

object Folding {

  object list_exercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldLeft()
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = ???
  }

}
