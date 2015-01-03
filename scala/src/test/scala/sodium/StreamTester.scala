package sodium

import java.util.ArrayList
import java.util.Arrays
import junit.framework.Assert._

import junit.framework.TestCase

class StreamTester extends TestCase {
  @Override
  protected def tearDown() {
    System.gc()
    Thread.sleep(100)
  }

  def testSendStream() {
    val e = new StreamSink[Int]()
    val out = new ArrayList[Int]()
    val l = e.listen(x => out.add(x))
    e.send(5)
    l.unlisten()
    assertEquals(Arrays.asList(5), out)
    e.send(6)
    assertEquals(Arrays.asList(5), out)
  }

  def testMap() {
    val e = new StreamSink[Int]()
    val m = e.map(x => x.toString)
    val out = new ArrayList[String]()
    val l = m.listen(x => out.add(x))
    e.send(5)
    l.unlisten()
    assertEquals(Arrays.asList("5"), out)
  }

  def testMergeNonSimultaneous() {
    val e1 = new StreamSink[Int]()
    val e2 = new StreamSink[Int]()
    val out = new ArrayList[Int]()
    val l = e1.merge(e2).listen(x => out.add(x))
    e1.send(7)
    e2.send(9)
    e1.send(8)
    l.unlisten()
    assertEquals(Arrays.asList(7, 9, 8), out)
  }

  def testMergeSimultaneous() {
    val e = new StreamSink[Int]()
    val out = new ArrayList[Int]()
    val l = e.merge(e).listen(x => out.add(x))
    e.send(7)
    e.send(9)
    l.unlisten()
    assertEquals(Arrays.asList(7, 7, 9, 9), out)
  }

  def testMergeLeftBias() {
    val e1 = new StreamSink[String]()
    val e2 = new StreamSink[String]()
    val out = new ArrayList[String]()
    val l = e1.merge(e2).listen(x => out.add(x))
    Transaction.doTransaction(_ => {
      e1.send("left1a")
      e1.send("left1b")
      e2.send("right1a")
      e2.send("right1b")
    })
    Transaction.doTransaction(_ => {
      e2.send("right2a")
      e2.send("right2b")
      e1.send("left2a")
      e1.send("left2b")
    })
    l.unlisten()
    assertEquals(Arrays.asList(
      "left1a", "left1b",
      "right1a", "right1b",
      "left2a", "left2b",
      "right2a", "right2b"), out)
  }

  def testCoalesce() {
    val e1 = new StreamSink[Int]()
    val e2 = new StreamSink[Int]()
    val out = new ArrayList[Int]()
    val l =
      e1.merge(e1.map(x => x * 100).merge(e2))
        .coalesce((a, b) => a + b)
        .listen((x) => { out.add(x) })
    e1.send(2)
    e1.send(8)
    e2.send(40)
    l.unlisten()
    assertEquals(Arrays.asList(202, 808, 40), out)
  }

  def testFilter() {
    val e = new StreamSink[Char]()
    val out = new ArrayList[Char]()
    val l = e.filter(c => Character.isUpperCase(c)).listen(c => out.add(c))
    List('H', 'o', 'I').foreach(e.send(_))
    l.unlisten()
    assertEquals(Arrays.asList('H', 'I'), out)
  }

  def testFilterNotNull() {
    val e = new StreamSink[String]()
    val out = new ArrayList[String]()
    val l = e.filterNotNull().listen(s => out.add(s))
    List("tomato", null, "peach").foreach(e.send(_))
    l.unlisten()
    assertEquals(Arrays.asList("tomato", "peach"), out)
  }

  def testFilterOptional() {
    val e = new StreamSink[Option[String]]()
    val out = new ArrayList[String]()
    val l = Stream.filterOptional(e).listen(s => out.add(s))
    List(Some("tomato"), None, Some("peach")).foreach(e.send(_))
    l.unlisten()
    assertEquals(Arrays.asList("tomato", "peach"), out)
  }

  def testLoopStream() {
    val ea = new StreamSink[Int]()
    val ec = Transaction.run[Stream[Int]](() => {
      val eb = new StreamLoop[Int]()
      val ec_ = ea.map(x => x % 10).merge(eb, (x, y) => x + y)
      val eb_out = ea.map(x => x / 10).filter(x => x != 0)
      eb.loop(eb_out)
      ec_
    })
    val out = new ArrayList[Int]()
    val l = ec.listen(x => out.add(x))
    ea.send(2)
    ea.send(52)
    l.unlisten()
    assertEquals(Arrays.asList(2, 7), out)
  }

  def testGate() {
    val ec = new StreamSink[Char]()
    val epred = new CellSink(true)
    val out = new ArrayList[Char]()
    val l = ec.gate(epred).listen(x => out.add(x))
    ec.send('H')
    epred.send(false)
    ec.send('O')
    epred.send(true)
    ec.send('I')
    l.unlisten()
    assertEquals(Arrays.asList('H', 'I'), out)
  }

  def testCollect() {
    val ea = new StreamSink[Int]()
    val out = new ArrayList[Int]()
    val sum = ea.collect[Int, Int](100, (a, s) => (a + s, a + s))
    val l = sum.listen(x => out.add(x))
    List(5, 7, 1, 2, 3).foreach(ea.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(105, 112, 113, 115, 118), out)
  }

  def testAccum() {
    val ea = new StreamSink[Int]()
    val out = new ArrayList[Int]()
    val sum = ea.accum[Int](100, (a, s) => a + s)
    val l = sum.updates().listen(x => out.add(x))
    List(5, 7, 1, 2, 3).foreach(ea.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(105, 112, 113, 115, 118), out)
  }

  def testOnce() {
    val e = new StreamSink[Char]()
    val out = new ArrayList[Char]()
    val l = e.once().listen(x => out.add(x))
    List('A', 'B', 'C').foreach(e.send(_))
    l.unlisten()
    assertEquals(Arrays.asList('A'), out)
  }

  def testDelay() {
    val e = new StreamSink[Char]()
    val b = e.hold(' ')
    val out = new ArrayList[Char]()
    val l = e.delay().snapshot(b).listen(x => out.add(x))
    List('C', 'B', 'A').foreach(e.send(_))
    l.unlisten()
    assertEquals(Arrays.asList('C', 'B', 'A'), out)
  }
}

