package experiments

import cats.Monoid
import cats.instances.string._
import cats.instances.int._
import cats.syntax.monoid._

object MonoidExamples {

  def reduce[A](as: List[A])(implicit monoid: Monoid[A]): A = as.foldLeft(monoid.empty)(monoid.combine)

  def reduceBetter[A: Monoid](as: List[A]): A = as.foldLeft(Monoid[A].empty)(_ |+| _)

  final case class ShoppingCart(items: List[String], totalPrice: Double)

  object ShoppingCart {
    implicit val naturalShoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
      ShoppingCart.empty,
      (s1, s2) => ShoppingCart(s1.items ::: s2.items, s1.totalPrice + s2.totalPrice)
    )

    def empty: ShoppingCart = ShoppingCart(Nil, 0)
  }

  def main(args: Array[String]): Unit = {

    println {
      reduce {
        1 :: 2 :: 3 :: Nil
      }
    }

    println {
      reduce {
        "1" :: "2" :: "3" :: Nil
      }
    }

    println {
      reduceBetter {
        ShoppingCart("Banana" :: "Chocolate" :: Nil, 15.5) ::
          ShoppingCart("T-shirt" :: "Trousers" :: Nil, 12.5) :: Nil
      }
    }

    implicit val phonebookMonoid: Monoid[Map[String, String]] = Monoid.instance(Map.empty, _ ++ _)

    println {
      reduceBetter {
        Map("A" -> "a") :: Map("B" -> "b") :: Nil
      }
    }

  }

}
