package sodium

import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.collection.mutable.ListBuffer

class StreamTester {
  @After
  def tearDown(): Unit = {
    System.gc()
    Thread.sleep(100)
  }

  @Test
  def testSendStream(): Unit = {
    val e = new StreamSink[Int]()
    val out = new ListBuffer[Int]()
    val l = e.listen(out.+=)
    e.send(5)
    l.unlisten()
    assertEquals(List(5), out)
    e.send(6)
    assertEquals(List(5), out)
  }

  @Test
  def testMap(): Unit = {
    val e = new StreamSink[Int]()
    val m = e.map(x => x.toString)
    val out = new ListBuffer[String]()
    val l = m.listen(out.+=)
    e.send(5)
    l.unlisten()
    assertEquals(List("5"), out)
  }

  @Test
  def testMapTo(): Unit = {
    val e = new StreamSink[Int]()
    val m = e.mapTo("fusebox")
    val out = new ListBuffer[String]()
    val l = m.listen(out.+=)
    e.send(5)
    e.send(6)
    l.unlisten()
    assertEquals(List("fusebox", "fusebox"), out)
  }

  @Test
  def testMergeNonSimultaneous(): Unit = {
    val e1 = new StreamSink[Int]()
    val e2 = new StreamSink[Int]()
    val out = new ListBuffer[Int]()
    val l = e2.orElse(e1).listen(out.+=)
    e1.send(7)
    e2.send(9)
    e1.send(8)
    l.unlisten()
    assertEquals(List(7, 9, 8), out)
  }

  @Test
  def testMergeSimultaneous(): Unit = {
    val s1 = new StreamSink[Int]((_, r) => r)
    val s2 = new StreamSink[Int]((_, r) => r)
    val out = new ListBuffer[Int]()
    val l = s2.orElse(s1).listen(out.+=)
    Transaction(_ => {
      s1.send(7)
      s2.send(60)
    })
    Transaction(_ => s1.send(9))
    Transaction(_ => {
      s1.send(7)
      s1.send(60)
      s2.send(8)
      s2.send(90)
    })
    Transaction(_ => {
      s2.send(8)
      s2.send(90)
      s1.send(7)
      s1.send(60)
    })
    Transaction(_ => {
      s2.send(8)
      s1.send(7)
      s2.send(90)
      s1.send(60)
    })
    l.unlisten()
    assertEquals(List(60, 9, 90, 90, 90), out)
  }

  @Test
  def testCoalesce(): Unit = {
    val s = new StreamSink[Int]((a, b) => a + b)
    val out = new ListBuffer[Int]()
    val l = s.listen(out.+=)
    Transaction(_ => s.send(2))
    Transaction(_ => {
      s.send(8)
      s.send(40)
    })
    l.unlisten()
    assertEquals(List(2, 48), out)
  }

  @Test
  def testFilter(): Unit = {
    val e = new StreamSink[Char]()
    val out = new ListBuffer[Char]()
    val l = e.filter(c => Character.isUpperCase(c)).listen(c => out.+=(c))
    List('H', 'o', 'I').foreach(e.send(_))
    l.unlisten()
    assertEquals(List('H', 'I'), out)
  }

  @Test
  def testFilterOptional(): Unit = {
    val e = new StreamSink[Option[String]]()
    val out = new ListBuffer[String]()
    val l = Stream.filterOptional(e).listen(s => out.+=(s))
    List(Some("tomato"), None, Some("peach")).foreach(e.send(_))
    l.unlisten()
    assertEquals(List("tomato", "peach"), out)
  }

  @Test
  def testLoopStream(): Unit = {
    val ea = new StreamSink[Int]()
    val ec = Transaction(_ => {
      val eb = new StreamLoop[Int]()
      val ec_ = ea.map(x => x % 10).merge(eb, (x, y) => x + y)
      val eb_out = ea.map(x => x / 10).filter(x => x != 0)
      eb.loop(eb_out)
      ec_
    })
    val out = new ListBuffer[Int]()
    val l = ec.listen(out.+=)
    List(2, 52).foreach(ea.send(_))
    l.unlisten()
    assertEquals(List(2, 7), out)
  }

  @Test
  def testGate(): Unit = {
    val ec = new StreamSink[Char]()
    val epred = new BehaviorSink(true)
    val out = new ListBuffer[Char]()
    val l = ec.gate(epred).listen(out.+=)
    ec.send('H')
    epred.send(false)
    ec.send('O')
    epred.send(true)
    ec.send('I')
    l.unlisten()
    assertEquals(List('H', 'I'), out)
  }

  @Test
  def testCollect(): Unit = {
    val ea = new StreamSink[Int]()
    val out = new ListBuffer[Int]()
    val sum = ea.collect[Int, Int](0, (a, s) => (a + s + 100, a + s))
    val l = sum.listen(out.+=)
    List(5, 7, 1, 2, 3).foreach(ea.send(_))
    l.unlisten()
    assertEquals(List(105, 112, 113, 115, 118), out)
  }

  @Test
  def testAccum(): Unit = {
    val ea = new StreamSink[Int]()
    val out = new ListBuffer[Int]()
    val sum = ea.accum[Int](100, (a, s) => a + s)
    val l = sum.listen(out.+=)
    List(5, 7, 1, 2, 3).foreach(ea.send(_))
    l.unlisten()
    assertEquals(List(100, 105, 112, 113, 115, 118), out)
  }

  @Test
  def testOnce(): Unit = {
    val e = new StreamSink[Char]()
    val out = new ListBuffer[Char]()
    val l = e.once().listen(out.+=)
    List('A', 'B', 'C').foreach(e.send(_))
    l.unlisten()
    assertEquals(List('A'), out)
  }

  @Test
  def testDefer(): Unit = {
    val e = new StreamSink[Char]()
    val b = e.hold(' ')
    val out = new ListBuffer[Char]()
    val l = Operational.defer(e).snapshot(b).listen(out.+=)
    List('C', 'B', 'A').foreach(e.send(_))
    l.unlisten()
    assertEquals(List('C', 'B', 'A'), out)
  }

  @Test
  def testStreamLoop(): Unit = {
    val ss = new StreamSink[Int]()
    val s = Transaction(_ => {
      val sl = new StreamLoop[Int]()
      val c = sl.map(v => v + 2).hold(0)
      val s2 = ss.snapshot(c, (x: Int, y: Int) => x + y)
      sl.loop(s2)
      s2
    })
    val out = new ListBuffer[Int]()
    val l = s.listen(out.+=)
    List(3, 4, 7, 8).foreach(ss.send(_))
    l.unlisten()
    assertEquals(List(3, 9, 18, 28), out)
  }

  @Test
  def testStreamLoopDefer(): Unit = {
    val ss = new StreamSink[Int]()
    val s = Transaction(_ => {
      val sl = new StreamLoop[Int]()
      val streamLocal = Operational.defer(ss.orElse(sl).filter(v => v < 5).map(v => v + 1))
      sl.loop(streamLocal)
      sl
    })
    val out = new ListBuffer[Int]()
    val l = s.listen(out.+=)
    ss.send(2)
    l.unlisten()
    assertEquals(List(3, 4, 5), out)
  }

}
