package sodium

import java.util.ArrayList
import java.util.Arrays

import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Test

class CellTester {
  import CellTester._

  @After def tearDown() {
    System.gc()
    Thread.sleep(100)
  }

  @Test
  def testHold() {
    val e = new StreamSink[Int]()
    val b = e.hold(0)
    val out = new ArrayList[Int]()
    val l = b.updates().listen(out.add(_))
    List(2, 9).foreach(e.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(2, 9), out)
  }

  @Test
  def testSnapshot() {
    val b = new CellSink[Int](0)
    val trigger = new StreamSink[Long]()
    val out = new ArrayList[String]()
    val l = trigger.snapshot[Int, String](b, (x, y) => x + " " + y).listen(out.add(_))
    trigger.send(100L)
    b.send(2)
    trigger.send(200L)
    b.send(9)
    b.send(1)
    trigger.send(300L)
    l.unlisten()
    assertEquals(Arrays.asList("100 0", "200 2", "300 1"), out)
  }

  @Test
  def testValues() {
    val b = new CellSink[Int](9)
    val out = new ArrayList[Int]()
    val l = b.value().listen(out.add(_))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(9, 2, 7), out)
  }

  @Test
  def testConstantBehavior() {
    val b = new Cell[Int](12)
    val out = new ArrayList[Int]()
    val l = b.value().listen(out.add(_))
    l.unlisten()
    assertEquals(Arrays.asList(12), out)
  }

  @Test
  def testValuesThenMap() {
    val b = new CellSink[Int](9)
    val out = new ArrayList[Int]()
    val l = b.value().map(x => x + 100).listen(out.add(_))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(109, 102, 107), out)
  }

  @Test
  def testValuesTwiceThenMap() {
    val b = new CellSink[Int](9)
    val out = new ArrayList[Int]()
    val l = doubleUp(b.value()).map(x => x + 100).listen(out.add(_))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(109, 109, 102, 102, 107, 107), out)
  }

  @Test
  def testValuesThenCoalesce() {
    val b = new CellSink[Int](9)
    val out = new ArrayList[Int]()
    val l = b.value().coalesce((fst, snd) => snd).listen(out.add(_))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(9, 2, 7), out)
  }

  @Test
  def testValuesTwiceThenCoalesce() {
    val b = new CellSink[Int](9)
    val out = new ArrayList[Int]()
    val l = doubleUp(b.value()).coalesce((fst, snd) => fst + snd).listen(out.add(_))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(18, 4, 14), out)
  }

  @Test
  def testValuesThenSnapshot() {
    val bi = new CellSink[Int](9)
    val bc = new CellSink[Character]('a')
    val out = new ArrayList[Character]()
    val l = bi.value().snapshot(bc).listen(out.add(_))
    bc.send('b')
    bi.send(2)
    bc.send('c')
    bi.send(7)
    l.unlisten()
    assertEquals(Arrays.asList('a', 'b', 'c'), out)
  }

  @Test
  def testValuesTwiceThenSnapshot() {
    val bi = new CellSink[Int](9)
    val bc = new CellSink[Character]('a')
    val out = new ArrayList[Character]()
    val l = doubleUp(bi.value()).snapshot(bc).listen(out.add(_))
    bc.send('b')
    bi.send(2)
    bc.send('c')
    bi.send(7)
    l.unlisten()
    assertEquals(Arrays.asList('a', 'a', 'b', 'b', 'c', 'c'), out)
  }

  @Test
  def testValuesThenMerge() {
    val bi = new CellSink[Int](9)
    val bj = new CellSink[Int](2)
    val out = new ArrayList[Int]()
    val l = bi.value().merge(bj.value(), (x, y) => x + y)
      .listen(out.add(_))
    bi.send(1)
    bj.send(4)
    l.unlisten()
    assertEquals(Arrays.asList(11, 1, 4), out)
  }

  @Test
  def testValuesThenFilter() {
    val b = new CellSink[Int](9)
    val out = new ArrayList[Int]()
    val l = b.value().filter(a => true).listen(out.add(_))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(9, 2, 7), out)
  }

  @Test
  def testValuesTwiceThenFilter() {
    val b = new CellSink[Int](9)
    val out = new ArrayList[Int]()
    val l = doubleUp(b.value()).filter(a => true).listen(out.add(_))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(9, 9, 2, 2, 7, 7), out)
  }

  @Test
  def testValuesThenOnce() {
    val b = new CellSink[Int](9)
    val out = new ArrayList[Int]()
    val l = b.value().once().listen(out.add(_))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(9), out)
  }

  @Test
  def testValuesTwiceThenOnce() {
    val b = new CellSink[Int](9)
    val out = new ArrayList[Int]()
    val l = doubleUp(b.value()).once().listen(out.add(_))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(9), out)
  }

  @Test
  def testValuesLateListen() {
    val b = new CellSink[Int](9)
    val out = new ArrayList[Int]()
    val value = b.value()
    b.send(8)
    val l = value.listen(out.add(_))
    b.send(2)
    l.unlisten()
    assertEquals(Arrays.asList(8, 2), out)
  }

  @Test
  def testMapB() {
    val b = new CellSink[Int](6)
    val out = new ArrayList[String]()
    val l = b.map(x => x.toString()).value().listen(out.add(_))
    b.send(8)
    l.unlisten()
    assertEquals(Arrays.asList("6", "8"), out)
  }

  def testMapBLateListen() {
    val b = new CellSink[Int](6)
    val out = new ArrayList[String]()
    val bm = b.map(x => x.toString())
    b.send(2)
    val l = bm.value().listen(out.add(_))
    b.send(8)
    l.unlisten()
    assertEquals(Arrays.asList("2", "8"), out)
  }

  @Test
  def testTransaction() {
    val calledBack = new Array[Boolean](1)
    Transaction.run((trans: Transaction) => {
      trans.prioritized(Node.NullNode, trans2 => { calledBack(0) = true })
    })
    assertEquals(true, calledBack(0))
  }

  @Test
  def testApply() {
    val bf = new CellSink[Long => String](b => "1 " + b)
    val ba = new CellSink[Long](5L)
    val out = new ArrayList[String]()
    val l = Cell.apply(bf, ba).value().listen(x => out.add(x))
    bf.send(b => "12 " + b)
    ba.send(6L)
    l.unlisten()
    assertEquals(Arrays.asList("1 5", "12 5", "12 6"), out)
  }

  @Test
  def testLift() {
    val a = new CellSink[Int](1)
    val b = new CellSink[Long](5L)
    val out = new ArrayList[String]()
    val l = Cell.lift[Int, Long, String]((x, y) => x + " " + y, a, b).value().listen(out.add(_))
    a.send(12)
    b.send(6L)
    l.unlisten()
    assertEquals(Arrays.asList("1 5", "12 5", "12 6"), out)
  }

  @Test
  def testLiftGlitch() {
    val a = new CellSink[Int](1)
    val a3 = a.map(x => x * 3)
    val a5 = a.map(x => x * 5)
    val b = Cell.lift[Int, Int, String]((x, y) => x + " " + y, a3, a5)
    val out = new ArrayList[String]()
    val l = b.value().listen(out.add(_))
    a.send(2)
    l.unlisten()
    assertEquals(Arrays.asList("3 5", "6 10"), out)
  }

  @Test
  def testHoldIsDelayed() {
    val e = new StreamSink[Int]()
    val h = e.hold(0)
    val pair = e.snapshot[Int, String](h, (a, b) => a + " " + b)
    val out = new ArrayList[String]()
    val l = pair.listen(out.add(_))
    e.send(2)
    e.send(3)
    l.unlisten()
    assertEquals(Arrays.asList("2 0", "3 2"), out)
  }

  @Test
  def testSwitchB() {
    val esb = new StreamSink[SB]()
    // Split each field out of SB so we can update multiple behaviours in a
    // single transaction.
    val ba = esb.map(s => s.a).filterNotNull().hold('A')
    val bb = esb.map(s => s.b).filterNotNull().hold('a')
    val bsw = esb.map(s => s.sw).filterNotNull().hold(ba)
    val bo = Cell.switchC(bsw)
    val out = new ArrayList[Character]()
    val l = bo.value().listen(c => out.add(c))
    esb.send(new SB('B', 'b', null))
    esb.send(new SB('C', 'c', bb))
    esb.send(new SB('D', 'd', null))
    esb.send(new SB('E', 'e', ba))
    esb.send(new SB('F', 'f', null))
    esb.send(new SB(null, null, bb))
    esb.send(new SB(null, null, ba))
    esb.send(new SB('G', 'g', bb))
    esb.send(new SB('H', 'h', ba))
    esb.send(new SB('I', 'i', ba))
    l.unlisten()
    assertEquals(Arrays.asList('A', 'B', 'c', 'd', 'E', 'F', 'f', 'F', 'g', 'H', 'I'), out)
  }

  @Test
  def testSwitchE() {
    val ese = new StreamSink[SE]()
    val ea = ese.map(s => s.a).filterNotNull()
    val eb = ese.map(s => s.b).filterNotNull()
    val bsw = ese.map(s => s.sw).filterNotNull().hold(ea)
    val out = new ArrayList[Char]()
    val eo = Cell.switchS(bsw)
    val l = eo.listen(c => out.add(c))
    ese.send(new SE('A', 'a', null))
    ese.send(new SE('B', 'b', null))
    ese.send(new SE('C', 'c', eb))
    ese.send(new SE('D', 'd', null))
    ese.send(new SE('E', 'e', ea))
    ese.send(new SE('F', 'f', null))
    ese.send(new SE('G', 'g', eb))
    ese.send(new SE('H', 'h', ea))
    ese.send(new SE('I', 'i', ea))
    l.unlisten()
    assertEquals(Arrays.asList('A', 'B', 'C', 'd', 'e', 'F', 'G', 'h', 'I'), out)
  }

  @Test
  def testLoopBehavior() {
    val ea = new StreamSink[Int]()
    val sum_out = Transaction.run[Cell[Int]](() => {
      val sum = new CellLoop[Int]()
      val sum_out_ = ea.snapshot[Int, Int](sum, (x, y) => x + y).hold(0)
      sum.loop(sum_out_)
      sum_out_
    })
    val out = new ArrayList[Int]()
    val l = sum_out.value().listen(out.add(_))
    List(2, 3, 1).foreach(ea.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(0, 2, 5, 6), out)
    assertEquals(6, sum_out.sample())
  }

  @Test
  def testCollect() {
    val ea = new StreamSink[Int]()
    val out = new ArrayList[Int]()
    val sum = ea.hold(100).collect[Int, Int](0, (a, s) => (a + s, a + s))
    val l = sum.value().listen(out.add(_))
    List(5, 7, 1, 2, 3).foreach(ea.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(100, 105, 112, 113, 115, 118), out)
  }

  @Test
  def testAccum() {
    val ea = new StreamSink[Int]()
    val out = new ArrayList[Int]()
    val sum = ea.accum[Int](100, (a, s) => a + s)
    val l = sum.value().listen(out.add(_))
    List(5, 7, 1, 2, 3).foreach(ea.send(_))
    l.unlisten()
    assertEquals(Arrays.asList(100, 105, 112, 113, 115, 118), out)
  }

  @Test
  def testLoopValueSnapshot() {
    val out = new ArrayList[String]()
    val eSnap = Transaction.run[Stream[String]](() => {
      val a = new Cell("lettuce")
      val b = new CellLoop[String]()
      val eSnap_ = a.value().snapshot[String, String](b, (aa, bb) => aa + " " + bb)
      b.loop(new Cell[String]("cheese"))
      eSnap_
    })
    val l = eSnap.listen((x) => { out.add(x) })
    l.unlisten()
    assertEquals(Arrays.asList("lettuce cheese"), out)
  }

  @Test
  def testLoopValueHold() {
    val out = new ArrayList[String]()
    val value = Transaction.run[Cell[String]](() => {
      val a = new CellLoop[String]()
      val value_ = a.value().hold("onion")
      a.loop(new Cell[String]("cheese"))
      value_
    })
    val eTick = new StreamSink[Int]()
    val l = eTick.snapshot(value).listen(out.add(_))
    eTick.send(0)
    l.unlisten()
    assertEquals(Arrays.asList("cheese"), out)
  }

  @Test
  def testLiftLoop() {
    val out = new ArrayList[String]()
    val b = new CellSink("kettle")
    val c = Transaction.run[Cell[String]](() => {
      val a = new CellLoop[String]()
      val c_ = Cell.lift[String, String, String]((aa, bb) => aa + " " + bb, a, b)
      a.loop(new Cell[String]("tea"))
      c_
    })
    val l = c.value().listen(out.add(_))
    b.send("caddy")
    l.unlisten()
    assertEquals(Arrays.asList("tea kettle", "tea caddy"), out)
  }
}

object CellTester {

  /**
   * This is used for tests where value() produces a single initial value on listen,
   * and then we double that up by causing that single initial event to be repeated.
   * This needs testing separately, because the code must be done carefully to achieve
   * this.
   */
  private def doubleUp(ev: Stream[Int]): Stream[Int] = ev.merge(ev)

  case class SB(val a: Character, val b: Character, val sw: Cell[Character])

  case class SE(val a: Character, val b: Character, val sw: Stream[Character])
}

