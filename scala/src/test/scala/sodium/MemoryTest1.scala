package sodium

object MemoryTest1 {
  def main(args: Array[String]) {
    new Thread() {
      def run() {
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
    val etens = et.map(x => x / 10)
    val changeTens = et.snapshot[Int, Int](t, (neu, old) =>
      if (neu.equals(old)) null else neu).filterNotNull
    val oout =
      changeTens.map(
        tens => t.map(tt => (tens, tt))).hold(t.map(tt => (0, tt)))
    val out = Cell.switchC(oout)
    val l = out.value().listen(tu => {
      //System.out.println(tu.a+","+tu.b)
    })
    var i = 0
    while (i < 1000000000) {
      et.send(i)
      i += 1
    }
    l.unlisten()
  }
}

