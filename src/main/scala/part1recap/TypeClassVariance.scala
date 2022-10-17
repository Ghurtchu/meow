package part1recap

object TypeClassVariance {

  import cats.Eq
  import cats.instances.int._ // Eq[Int]
  import cats.instances.option._ // construct Eq[Option[Int]]
  import cats.syntax.eq._ // syntax

  val aComparison = Option(2) === Option(3)
  // val anInvalidComparison = Some(2) === None // Eq[Some[Int]] not found, even though Some <: Option

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+A]

  val cage: Cage[Animal] = new Cage[Cat] // if cat extends animal then cage[cat] <: cage[animal]

  // contravariant type: subtyping is propagated backwards
  class Vet[-A]

  val contravariantCage: Vet[Cat] = new Vet[Animal]

  // rule of thumb: "HAS a T" = covariant, "ACTS on T" = contravariant
  // variance affects how TypeClass instances are fetched

  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow!") //

  makeSound[Animal] // that is ok, compiler can find AnimalSoundMaker
  makeSound[Cat] // also ok, will take AnimalSoundMaker too

  // has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // covariant TC

  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "Animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats..."
  }

  def organizeShow[A](implicit event: AnimalShow[A]): String = event.show

  println(organizeShow[Cat]) // will use CatShow

  // covariant type classes will always use the more specific type class instances

  def main(args: Array[String]): Unit = {

    // better approach = use invariant instances with smart constructors
    Option.apply[Int](10) === Option.empty[Int] // Same as Some(2) === None

  }

}
