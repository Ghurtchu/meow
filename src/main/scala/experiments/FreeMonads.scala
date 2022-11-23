package experiments

object FreeMonads {

  // capabilities:
  // pure(a) => M[A]
  // flatMap(A => M[B]): M[B]

  trait functor[f[_]] {
    def map[a, b](fa: f[a])(f: a => b): f[b]
  }

  trait monad[f[_]] extends functor[f] {
    def pure[a](a: a): f[a]
    def flatmap[a, b](fa: f[a])(f: a => f[b]): f[b]
  }

  type option[A] = Option[A]

  implicit val option_monad: monad[option] = new monad[option] {
    override def pure[a](a: a): option[a] = Option(a)
    override def map[a, b](fa: option[a])(f: a => b): option[b] = fa map f
    override def flatmap[a, b](fa: option[a])(f: a => option[b]): option[b] = fa flatMap f
  }

  def zip[f[_], a, b](fa: f[a], fb: f[b])(implicit monad: monad[f]): f[(a, b)] =
    monad.flatmap(fa)(a => monad.map(fb)(b => (a, b)))

  type int = Int
  type str = String

  zip[option, int, str](Option(10), Option(20)) // Some((10, 20))


  trait free[f[_], a] {
    def flatmap[b](f: a => free[f, b]): free[f, b]
    def map[b](f: a => b): free[f, b] = flatmap(a => free.pure(f(a)))
  }

  object free {
    def pure[f[_], a](a: a): free[f, a] = ???
    def lifF[f[_], a](fa: f[a]): free[f, a] = ???
  }

  // sequence computations as data structures and then attach monadic type at the end

  trait dbops[a] {

  }

  type unit = Unit

  final case class create[a](key: str, value: a) extends dbops[unit]
  final case class read[a](key: str)             extends dbops[a]
  final case class update[a](key: str, value: a) extends dbops[a]
  final case class delete(key: str)              extends dbops[unit]

  // definitions
  type dbmonad[a] = free[dbops, a]



  def main(args: Array[String]): Unit = {

  }

}
