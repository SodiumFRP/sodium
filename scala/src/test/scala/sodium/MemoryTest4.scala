package sodium

object MemoryTest4 {
  def main(args: Array[String]): Unit = {
    new Thread() {
      override def run(): Unit = {
        try {
          while (true) {
            println(s"memory ${Runtime.getRuntime().totalMemory()}")
            Thread.sleep(5000)
          }
        } catch {
          case e: InterruptedException => e.printStackTrace()
        }
      }
    }.start()

    val et = new Stream[Int]()
    val eChange = new StreamSink[Int]()
    val oout = eChange.map(_ => et).hold(et)
    val out = Cell.switchS[Int](oout)
    val l = out.listen(tt => {
      println(tt)
    })
    var i = 0
    while (i < 1000000000) {
      eChange.send(i)
      i += 1
    }
    l.unlisten()
  }
}
