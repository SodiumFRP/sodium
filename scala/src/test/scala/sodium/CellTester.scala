package sodium

import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.collection.mutable.ListBuffer

class CellTester {
  import CellTester._

  @After def tearDown(): Unit = {
    System.gc()
    Thread.sleep(100)
  }

  @Test
  def testHold(): Unit = {
    val e = new StreamSink[Int]()
    val b = e.hold(0)
    val out = new ListBuffer[Int]()
    val l = Operational.updates(b).listen(out.+=(_))
    List(2, 9).foreach(e.send(_))
    l.unlisten()
    assertEquals(List(2, 9), out)
  }

  @Test
  def testSnapshot(): Unit = {
    val b = new CellSink(0)
    val trigger = new StreamSink[Long]()
    val out = new ListBuffer[String]()
    val l = trigger.snapshot[Int, String](b, (x, y) => x + " " + y).listen(out.+=(_))
    trigger.send(100L)
    b.send(2)
    trigger.send(200L)
    b.send(9)
    b.send(1)
    trigger.send(300L)
    l.unlisten()
    assertEquals(List("100 0", "200 2", "300 1"), out)
  }

  @Test
  def testValues(): Unit = {
    val b = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = b.listen(out.+=(_))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(List(9, 2, 7), out)
  }

  @Test
  def testConstantBehavior(): Unit = {
    val b = new Cell(12)
    val out = new ListBuffer[Int]()
    val l = b.listen(out.+=(_))
    l.unlisten()
    assertEquals(List(12), out)
  }

  @Test
  def testValueThenMap(): Unit = {
    val b = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = Transaction(_ => Operational.value(b).map(x => x + 100).listen(out.+=(_)))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(List(109, 102, 107), out)
  }

  @Test
  def testValuesTwiceThenMap(): Unit = {
    val b = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = Transaction(_ => doubleUp(Operational.value(b)).map(x => x + 100).listen(out.+=(_)))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(List(109, 109, 102, 102, 107, 107), out)
  }

  @Test
  def testValuesThenCoalesce(): Unit = {
    val b = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = Transaction(_ => Operational.value(b).coalesce((fst, snd) => snd).listen(out.+=(_)))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(List(9, 2, 7), out)
  }

  @Test
  def testValuesTwiceThenCoalesce(): Unit = {
    val b = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = Transaction(_ => doubleUp(Operational.value(b)).coalesce((fst, snd) => fst + snd).listen(out.+=(_)))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(List(18, 4, 14), out)
  }

  @Test
  def testValuesThenSnapshot(): Unit = {
    val bi = new CellSink(9)
    val bc = new CellSink('a')
    val out = new ListBuffer[Character]()
    val l = Transaction(_ => Operational.value(bi).snapshot(bc).listen(out.+=(_)))
    bc.send('b')
    bi.send(2)
    bc.send('c')
    bi.send(7)
    l.unlisten()
    assertEquals(List('a', 'b', 'c'), out)
  }

  @Test
  def testValuesTwiceThenSnapshot(): Unit = {
    val bi = new CellSink(9)
    val bc = new CellSink('a')
    val out = new ListBuffer[Character]()
    val l = Transaction(_ => doubleUp(Operational.value(bi)).snapshot(bc).listen(out.+=(_)))
    bc.send('b')
    bi.send(2)
    bc.send('c')
    bi.send(7)
    l.unlisten()
    assertEquals(List('a', 'a', 'b', 'b', 'c', 'c'), out)
  }

  @Test
  def testValuesThenMerge(): Unit = {
    val bi = new CellSink(9)
    val bj = new CellSink(2)
    val out = new ListBuffer[Int]()
    val l = Transaction(_ => Operational.value(bi).merge(Operational.value(bj), (x, y) => x + y).listen(out.+=(_)))
    bi.send(1)
    bj.send(4)
    l.unlisten()
    assertEquals(List(11, 1, 4), out)
  }

  @Test
  def testValuesThenFilter(): Unit = {
    val b = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = Transaction(_ => Operational.value(b).filter(a => true).listen(out.+=(_)))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(List(9, 2, 7), out)
  }

  @Test
  def testValuesTwiceThenFilter(): Unit = {
    val b = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = Transaction(_ => doubleUp(Operational.value(b)).filter(a => true).listen(out.+=(_)))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(List(9, 9, 2, 2, 7, 7), out)
  }

  @Test
  def testValuesThenOnce(): Unit = {
    val b = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = Transaction(_ => Operational.value(b).once().listen(out.+=(_)))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(List(9), out)
  }

  @Test
  def testValuesTwiceThenOnce(): Unit = {
    val b = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = Transaction(_ => doubleUp(Operational.value(b)).once().listen(out.+=(_)))
    List(2, 7).foreach(b.send(_))
    l.unlisten()
    assertEquals(List(9), out)
  }

  @Test
  def testValuesLateListen(): Unit = {
    val b = new CellSink(9)
    val out = new ListBuffer[Int]()
    val value = Operational.value(b)
    b.send(8)
    val l = value.listen(out.+=(_))
    b.send(2)
    l.unlisten()
    assertEquals(List(2), out)
  }

  @Test
  def testMapB(): Unit = {
    val b = new CellSink(6)
    val out = new ListBuffer[String]()
    val l = b.map(x => x.toString()).listen(out.+=(_))
    b.send(8)
    l.unlisten()
    assertEquals(List("6", "8"), out)
  }

  def testMapBLateListen(): Unit = {
    val b = new CellSink(6)
    val out = new ListBuffer[String]()
    val bm = b.map(x => x.toString())
    b.send(2)
    val l = bm.listen(out.+=(_))
    b.send(8)
    l.unlisten()
    assertEquals(List("2", "8"), out)
  }

  @Test
  def testTransaction(): Unit = {
    var calledBack = false
    Transaction(trans => trans.prioritized(Node.NullNode, trans2 => calledBack = true))
    assertEquals(true, calledBack)
  }

  @Test
  def testApply(): Unit = {
    val bf = new CellSink[Long => String](b => "1 " + b)
    val ba = new CellSink(5L)
    val out = new ListBuffer[String]()
    val l = Cell(bf, ba).listen(x => out.+=(x))
    bf.send(b => "12 " + b)
    ba.send(6L)
    l.unlisten()
    assertEquals(List("1 5", "12 5", "12 6"), out)
  }

  @Test
  def testLift(): Unit = {
    val a = new CellSink(1)
    val b = new CellSink(5L)
    val out = new ListBuffer[String]()
    val l = Cell.lift[Int, Long, String]((x, y) => x + " " + y, a, b).listen(out.+=(_))
    a.send(12)
    b.send(6L)
    l.unlisten()
    assertEquals(List("1 5", "12 5", "12 6"), out)
  }

  @Test
  def testLiftGlitch(): Unit = {
    val a = new CellSink(1)
    val a3 = a.map(x => x * 3)
    val a5 = a.map(x => x * 5)
    val out = new ListBuffer[String]()
    val l = Cell.lift[Int, Int, String]((x, y) => x + " " + y, a3, a5).listen(out.+=(_))
    a.send(2)
    l.unlisten()
    assertEquals(List("3 5", "6 10"), out)
  }

  @Test
  def testLiftFromSimultaneous(): Unit = {
    val t: ((CellSink[Int], CellSink[Int])) = Transaction(trans => {
      val b1 = new CellSink(3)
      val b2 = new CellSink(5)
      b2.send(7)
      (b1, b2)
    })
    val b1 = t._1
    val b2 = t._2
    val out = new ListBuffer[Int]()
    val l = Cell
      .lift((x: Int, y: Int) => x + y, b1, b2)
      .listen((x: Int) => out.+=(x))
    l.unlisten()
    assertEquals(List(10), out)
  }

  @Test
  def testHoldIsDelayed(): Unit = {
    val e = new StreamSink[Int]()
    val h = e.hold(0)
    val out = new ListBuffer[String]()
    val l = e.snapshot[Int, String](h, (a, b) => a + " " + b).listen(out.+=(_))
    List(2, 3).foreach(e.send(_))
    l.unlisten()
    assertEquals(List("2 0", "3 2"), out)
  }

  @Test
  def testSwitchB(): Unit = {
    val esb = new StreamSink[SB]()
    // Split each field out of SB so we can update multiple behaviours in a
    // single transaction.
    val ba = Stream.filterOptional(esb.map(s => s.a)).hold('A')
    val bb = Stream.filterOptional(esb.map(s => s.b)).hold('a')
    val bsw = Stream.filterOptional(esb.map(s => s.sw)).hold(ba)
    val bo = Cell.switchC(bsw)
    val out = new ListBuffer[Character]()
    val l = bo.listen(out.+=(_))
    List(
      new SB(Some('B'), Some('b'), None),
      new SB(Some('C'), Some('c'), Some(bb)),
      new SB(Some('D'), Some('d'), None),
      new SB(Some('E'), Some('e'), Some(ba)),
      new SB(Some('F'), Some('f'), None),
      new SB(None, None, Some(bb)),
      new SB(None, None, Some(ba)),
      new SB(Some('G'), Some('g'), Some(bb)),
      new SB(Some('H'), Some('h'), Some(ba)),
      new SB(Some('I'), Some('i'), Some(ba))
    ).foreach(esb.send(_))
    l.unlisten()
    assertEquals(List('A', 'B', 'c', 'd', 'E', 'F', 'f', 'F', 'g', 'H', 'I'), out)
  }

  @Test
  def testSwitchE(): Unit = {
    val ese = new StreamSink[SE]()
    val ea = ese.map(s => s.a)
    val eb = ese.map(s => s.b)
    val bsw = Stream.filterOptional(ese.map(s => s.sw)).hold(ea)
    val out = new ListBuffer[Char]()
    val eo = Cell.switchS(bsw)
    val l = eo.listen(out.+=(_))
    List(
      new SE('A', 'a', None),
      new SE('B', 'b', None),
      new SE('C', 'c', Some(eb)),
      new SE('D', 'd', None),
      new SE('E', 'e', Some(ea)),
      new SE('F', 'f', None),
      new SE('G', 'g', Some(eb)),
      new SE('H', 'h', Some(ea)),
      new SE('I', 'i', Some(ea))
    ).foreach(ese.send(_))
    l.unlisten()
    assertEquals(List('A', 'B', 'C', 'd', 'e', 'F', 'G', 'h', 'I'), out)
  }

  @Test
  def testLoopBehavior(): Unit = {
    val ea = new StreamSink[Int]()
    val sum_out = Transaction(_ => {
      val sum = new CellLoop[Int]()
      val sum_out_ = ea.snapshot[Int, Int](sum, (x, y) => x + y).hold(0)
      sum.loop(sum_out_)
      sum_out_
    })
    val out = new ListBuffer[Int]()
    val l = sum_out.listen(out.+=(_))
    List(2, 3, 1).foreach(ea.send(_))
    l.unlisten()
    assertEquals(List(0, 2, 5, 6), out)
    assertEquals(6.toLong, sum_out.sample().toLong)
  }

  @Test
  def testAccum(): Unit = {
    val ea = new StreamSink[Int]()
    val out = new ListBuffer[Int]()
    val sum = ea.accum[Int](100, (a, s) => a + s)
    val l = sum.listen(out.+=(_))
    List(5, 7, 1, 2, 3).foreach(ea.send(_))
    l.unlisten()
    assertEquals(List(100, 105, 112, 113, 115, 118), out)
  }

  @Test
  def testLoopValueSnapshot(): Unit = {
    val out = new ListBuffer[String]()
    val l: Listener = Transaction(_ => {
      val a = new Cell("lettuce")
      val b = new CellLoop[String]()
      val eSnap = Operational.value(a).snapshot[String, String](b, (aa, bb) => aa + " " + bb)
      b.loop(new Cell[String]("cheese"))
      eSnap.listen(out.+=(_))
    })

    l.unlisten()
    assertEquals(List("lettuce cheese"), out)
  }

  @Test
  def testLoopValueHold(): Unit = {
    val out = new ListBuffer[String]()
    val value = Transaction(_ => {
      val a = new CellLoop[String]()
      val value_ = Operational.value(a).hold("onion")
      a.loop(new Cell[String]("cheese"))
      value_
    })
    val eTick = new StreamSink[Int]()
    val l = eTick.snapshot(value).listen(out.+=(_))
    eTick.send(0)
    l.unlisten()
    assertEquals(List("cheese"), out)
  }

  @Test
  def testLiftLoop(): Unit = {
    val out = new ListBuffer[String]()
    val b = new CellSink("kettle")
    val c = Transaction(_ => {
      val a = new CellLoop[String]()
      val c_ = Cell.lift[String, String, String]((aa, bb) => aa + " " + bb, a, b)
      a.loop(new Cell[String]("tea"))
      c_
    })
    val l = c.listen(out.+=(_))
    b.send("caddy")
    l.unlisten()
    assertEquals(List("tea kettle", "tea caddy"), out)
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
}

case class SB(val a: Option[Character], val b: Option[Character], val sw: Option[Cell[Character]])

case class SE(val a: Character, val b: Character, val sw: Option[Stream[Character]])
