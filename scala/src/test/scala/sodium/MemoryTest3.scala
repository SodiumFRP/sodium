package sodium

object MemoryTest3 {
  def main(args: Array[String]) {
    new Thread() {
      override def run() {
        try {
          while (true) {
            println("memory " + Runtime.getRuntime().totalMemory())
            Thread.sleep(5000)
          }
        } catch {
          case e: InterruptedException => e.printStackTrace
        }
      }
    }.start()

    val et = new StreamSink[Int]()
    val t = et.hold(0)
    val eChange = new StreamSink[Int]()
    val oout = eChange.map(x => t).hold(t)
    val out = Cell.switchC(oout)
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
