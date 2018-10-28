package sodium

object MemoryTest1 {
  def main(args: Array[String]): Unit = {
    new Thread() {
      override def run(): Unit = {
        try {
          while (true) {
            println(s"memory ${Runtime.getRuntime().totalMemory()}")
            Thread.sleep(5000)
          }
        } catch {
          case e: InterruptedException => e.printStackTrace
        }
      }
    }.start()

    val et = new StreamSink[Int]()
    val t = et.hold(0)
    et.map(x => x / 10)
    val changeTens =
      Stream.filterOptional(
        et.snapshot[Int, Option[Int]](t, (neu: Int, old: Int) => if (neu.equals(old)) None else Some(neu)))
    val oout =
      changeTens.map(tens => t.map(tt => (tens, tt))).hold(t.map(tt => (0, tt)))
    val out = Cell.switchC(oout)
    val l = out.listen(tu => {
      println(s"${tu._1},${tu._2}")
    })
    var i = 0
    while (i < 1000000000) {
      et.send(i)
      i += 1
    }
    l.unlisten()
  }
}
