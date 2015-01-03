package sodium

object MemoryTest5 {
  def main(args: Array[String]) {
    new Thread() {
      override def run() {
        try {
          while (true) {
            println("memory " + Runtime.getRuntime().totalMemory())
            Thread.sleep(5000)
          }
        } catch {
          case e: InterruptedException => e.printStackTrace()
        }
      }
    }.start()

    val eChange = new StreamSink[Int]()
    val out = eChange.hold(0)
    val l = out.value().listen(tt => {
      //System.out.println(tt)
    })
    var i = 0
    while (i < 1000000000) {
      eChange.send(i)
      i += 1
    }
    l.unlisten()
  }
}
