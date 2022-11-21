package part3datamanipulation

object Writers {

  import cats.data.Writer

  // define them at the start
  // manipulate them with pure FP functions
  // in the end you either dump logs or value

  // Writer[L, Value], L aka Logs type
  val writer: Writer[List[String], Int] = Writer(
    List("Started something"),
    42
  )
  val mappedValue = writer.map(_ + 2) // same log, value = 44
  val mappedLog = writer.mapWritten(l => "What?!" :: l)

  val mappedBoth = writer.bimap(
    l => "Did something" :: l,
    _ * 2
  )

  val mappedBoth2 = writer.mapBoth { (logs, int) => ("Did something great again" :: logs, int * 2) }

  val writerA: Writer[Vector[String], Int] = Writer(
    Vector("Log A1", "Log A2"), 10)

  val writerB: Writer[Vector[String], Int] = Writer(Vector("Log B1"), 40)

  import cats.instances.vector._ // will import semigroup of vector, natural combination is just concatenation

  val composite = for {
    a <- writerA
    b <- writerB
  } yield (a, b)

  def main(args: Array[String]): Unit = {

    val value: Int = mappedBoth.value
    val logs: List[String] = mappedBoth.written
    val (l, r) = mappedBoth.run
    println((l, r))
    assert(r == 84)

    println(composite.run)

  }

}
