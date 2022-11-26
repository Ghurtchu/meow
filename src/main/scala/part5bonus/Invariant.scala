package part5bonus

object Invariant {

  trait Crypto[A] {
    def encrypt(a: A): String // from -> to
    def decrypt(s: String): A // to   -> from
  }

  def encrypt[A](a: A)(implicit crypto: Crypto[A]): String = crypto encrypt a
  def decrypt[A](s: String)(implicit crypto: Crypto[A]): A = crypto decrypt s

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(a: String): String = a.map(c => (c + 2).toChar)
    override def decrypt(s: String): String = s.map(c => (c - 2).toChar)
  }

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Let's encrypt")
    val decrypted = decrypt(encrypted)

    println(encrypted)
    println(decrypted)
  }

}
