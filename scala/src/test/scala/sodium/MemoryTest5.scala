package sodium

object MemoryTest5 {
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

    val eChange = new StreamSink[Int]()
    val out = eChange.hold(0)
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
