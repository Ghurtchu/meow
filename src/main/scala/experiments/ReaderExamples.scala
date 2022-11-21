package experiments

import cats.data.Reader

object ReaderExamples {

  // build Hero using Config
  // steps: Config -> Person -> Character -> Hero

  sealed trait Strength

  case object Intelligence extends Strength
  case object Power        extends Strength
  case object Perseverance extends Strength

  final case class Level(value: Byte)

  final case class Config(age: Int, name: String, strength: Strength, level: Level)

  final case class Person(age: Int, name: String)
  final case class Character(person: Person, strength: Strength)
  final case class Hero(character: Character, level: Level)

  // build person
  // use person to build Character

  def main(args: Array[String]): Unit = {

    import cats.data.Reader

    val config: Config = Config(28, "Nika", Intelligence, Level(20))

    val personReader: Reader[Config, Person] = Reader(cfg => Person(cfg.age, cfg.name))

    val heroVerbose =
      personReader
        .flatMap(person => Reader[Config, Character](cfg => Character(person, cfg.strength)))
        .flatMap(character => Reader[Config, Hero](cfg => Hero(character, cfg.level)))


    val heroKleisli = for {
      person <- personReader
      character <- Reader[Config, Character](cfg => Character(person, cfg.strength))
      hero <- Reader[Config, Hero](cfg => Hero(character, cfg.level))
    } yield hero

    val hero: Hero = heroKleisli.run(config)

    println(heroVerbose.run(config))
    println(hero)

  }


}
