package experiments

import cats.Semigroup // for building Semigroup instances
import cats.instances.int._ // import int instances for all type classes
import cats.syntax.semigroup._

object SemigroupExamples {

  final case class Data(x: Int, y: Int)

  object Data {
    implicit val dataSemigroup: Semigroup[Data] = Semigroup.instance[Data]((d1, d2) => Data(d1.x + d2.x, d1.y + d2.y))
  }

  object CharInstances {
    implicit val charSemigroup: Semigroup[Char] = Semigroup.instance[Char]((c1, c2) => (c1.toInt + c2.toInt).toChar)
  }

  def reduce[A: Semigroup](as: List[A]): A = as.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {

    println {
      reduce {
        1 :: 2 :: 3 :: Nil
      }
    }

    println {
      reduce {
        Data(1, 1) :: Data(2, 2) :: Data(3, 3) :: Nil
      }
    }

    import CharInstances._

    println {
      reduce {
        '1' :: '2' :: '3' :: '4' :: '5' :: Nil
      }
    }

  }

}
