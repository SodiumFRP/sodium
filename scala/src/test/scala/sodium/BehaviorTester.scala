package sodium

import org.junit.Assert.assertEquals
import org.junit.{After, Test}

import scala.collection.mutable.ListBuffer

class BehaviorTester {
  import BehaviorTester._

  @After def tearDown(): Unit = {
    System.gc()
    Thread.sleep(100)
  }

  //from C# implmentation
  @Test
  def testHold(): Unit = {
    val s = new StreamSink[Int]()
    val c = s.hold(0)
    val out = new ListBuffer[Int]()
    val l = c.listen(out.+=)
    List(2, 9).foreach(s.send)
    l.unlisten()
    assertEquals(List(0, 2, 9), out)
  }

  //from C# implmentation
  @Test
  def testSendNull(): Unit = {
    val c = new CellSink[String]("")
    val out = new ListBuffer[String]()
    val l = c.listen(out.+=)
    c.send("0")
    c.send(null)
    c.send("1")
    l.unlisten
    assertEquals(List("", "0", null, "1"), out)
  }

  @Test
  def testHoldUpdates(): Unit = {
    val e = new StreamSink[Int]()
    val b = e.hold(0)
    val out = new ListBuffer[Int]()
    val l = b.updates().listen(out.+=)
    List(2, 9).foreach(e.send)
    l.unlisten()
    assertEquals(List(2, 9), out)
  }

  @Test
  def testSnapshot(): Unit = {
    val b: BehaviorSink[Int] = new BehaviorSink(0)
    val trigger = new StreamSink[Long]()
    val out = new ListBuffer[String]()
    val l = trigger.snapshot[Int, String](b, (x: Long, y: Int) => x + " " + y).listen(out.+=)
    trigger.send(100L)
    b.send(2)
    trigger.send(200L)
    b.send(9)
    b.send(1)
    trigger.send(300L)
    l.unlisten()
    assertEquals(List("100 0", "200 2", "300 1"), out)
  }

  //from C# implementation
  @Test
  def testListen(): Unit = {
    val c = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = c.listen(out.+=)
    List(2, 7).foreach(c.send)
    l.unlisten()
    assertEquals(List(9, 2, 7), out)
  }

  //from C# implementation
  @Test
  def testListenOnce(): Unit = {
    val b = new BehaviorSink(9)
    val out = new ListBuffer[Int]()
    val l = Transaction { _ =>
      Operational.value(b).listenOnce(out.+=)
    }
    List(2, 7).foreach(b.send)
    l.unlisten()
    assertEquals(List(9), out)
  }

  //from C# implementation
  @Test
  def testUpdates(): Unit = {
    val b = new BehaviorSink(9)
    val out = new ListBuffer[Int]()
    val l = Operational.updates(b).listen(out.+=)
    List(2, 7).foreach(b.send)
    l.unlisten()
    assertEquals(List(2, 7), out)
  }

  //from C# implementation
  @Test
  def testCellUpdates(): Unit = {
    val c = new CellSink(9)
    val out = new ListBuffer[Int]()
    val l = c.updates().listen(out.+=)
    List(2, 7).foreach(c.send)
    l.unlisten()
    assertEquals(List(2, 7), out)
  }

  @Test
  def testValues(): Unit = {
    val b = new BehaviorSink(9)
    val out = new ListBuffer[Int]()
    val l = b.listen(out.+=)
    List(2, 7).foreach(b.send)
    l.unlisten()
    assertEquals(List(9, 2, 7), out)
  }

  //from C# implementation
  @Test
  def testCellLoop(): Unit = {
    val s = new StreamSink[Int]()
    val cell: Cell[Int] = Transaction(_ => {
      val cellLoop: CellLoop[Int] = new CellLoop[Int]()
      val cellLocal: Cell[Int] = s.snapshot[Int, Int](cellLoop, (x: Int, y: Int) => x + y).hold(1)
      cellLoop.loop(cellLocal)
      cellLocal
    })
    val out = new ListBuffer[Int]()
    val l = cell.listen(out.+=)
    List(3, 4, 7, 8).foreach(s.send)
    l.unlisten()
    assertEquals(List(1, 4, 8, 15, 23), out)
  }

  @Test
  def testConstantBehavior(): Unit = {
    val b = new Behavior(12)
    val out = new ListBuffer[Int]()
    val l = b.listen(out.+=)
    l.unlisten()
    assertEquals(List(12), out)
  }

  @Test
  def testMap(): Unit = {
    val b = new BehaviorSink(6)
    val out = new ListBuffer[String]()
    val l = b.map(_.toString()).listen(out.+=)
    b.send(8)
    l.unlisten()
    assertEquals(List("6", "8"), out)
  }

  def testMapCLateListen(): Unit = {
    val b = new BehaviorSink(6)
    val out = new ListBuffer[String]()
    val bm = b.map(_.toString())
    b.send(2)
    val l = bm.listen(out.+=)
    b.send(8)
    l.unlisten()
    assertEquals(List("2", "8"), out)
  }

  @Test
  def testApply(): Unit = {
    val bf = new BehaviorSink[Long => String](b => "1 " + b)
    val ba = new BehaviorSink(5L)
    val out = new ListBuffer[String]()
    val l = Behavior(bf, ba).listen(out.+=)
    bf.send(b => "12 " + b)
    ba.send(6L)
    l.unlisten()
    assertEquals(List("1 5", "12 5", "12 6"), out)
  }

  @Test
  def testLift(): Unit = {
    val a = new BehaviorSink(1)
    val b = new BehaviorSink(5L)
    val out = new ListBuffer[String]()
    val l = a.lift[Long, String](b, (x, y) => x + " " + y).listen(out.+=)
    a.send(12)
    b.send(6L)
    l.unlisten()
    assertEquals(List("1 5", "12 5", "12 6"), out)
  }

  @Test
  def testLiftGlitch(): Unit = {
    val a = new BehaviorSink(1)
    val a3 = a.map(x => x * 3)
    val a5 = a.map(x => x * 5)
    val out = new ListBuffer[String]()
    val l = a3.lift[Int, String](a5, (x, y) => x + " " + y).listen(out.+=)
    a.send(2)
    l.unlisten()
    assertEquals(List("3 5", "6 10"), out)
  }

  @Test
  def testLiftFromSimultaneous(): Unit = {
    val t: ((BehaviorSink[Int], BehaviorSink[Int])) = Transaction(_ => {
      val b1 = new BehaviorSink(3)
      val b2 = new BehaviorSink(5)
      b2.send(7)
      (b1, b2)
    })
    val b1 = t._1
    val b2 = t._2
    val out = new ListBuffer[Int]()
    val l = b1
      .lift(b2, (x: Int, y: Int) => x + y)
      .listen((x: Int) => out.+=(x))
    l.unlisten()
    assertEquals(List(10), out)
  }

  @Test
  def testHoldIsDelayed(): Unit = {
    val e = new StreamSink[Int]()
    val h = e.hold(0)
    val out = new ListBuffer[String]()
    val l = e.snapshot[Int, String](h, (a: Int, b: Int) => a + " " + b).listen(out.+=)
    List(2, 3).foreach(e.send)
    l.unlisten()
    assertEquals(List("2 0", "3 2"), out)
  }

  @Test
  def testSwitchC(): Unit = {
    val esb = new StreamSink[SB]()
    // Split each field out of SB so we can update multiple behaviours in a
    // single transaction.
    val ba = Stream.filterOptional(esb.map(s => s.a)).hold('A')
    val bb = Stream.filterOptional(esb.map(s => s.b)).hold('a')
    val bsw = Stream.filterOptional(esb.map(s => s.sw)).hold(ba)
    val bo = Cell.switchC(bsw)
    val out = new ListBuffer[Character]()
    val l = bo.listen(out.+=)
    List(
      SB(Some('B'), Some('b'), None),
      SB(Some('C'), Some('c'), Some(bb)),
      SB(Some('D'), Some('d'), None),
      SB(Some('E'), Some('e'), Some(ba)),
      SB(Some('F'), Some('f'), None),
      SB(None, None, Some(bb)),
      SB(None, None, Some(ba)),
      SB(Some('G'), Some('g'), Some(bb)),
      SB(Some('H'), Some('h'), Some(ba)),
      SB(Some('I'), Some('i'), Some(ba))
    ).foreach(esb.send)
    l.unlisten()
    assertEquals(List('A', 'B', 'c', 'd', 'E', 'F', 'f', 'F', 'g', 'H', 'I'), out)
  }

  @Test
  def testSwitchS(): Unit = {
    val ese = new StreamSink[SE]()
    val ea = ese.map(s => s.a)
    val eb = ese.map(s => s.b)
    val bsw = Stream.filterOptional(ese.map(s => s.sw)).hold(ea)
    val out = new ListBuffer[Char]()
    val eo = Cell.switchS(bsw)
    val l = eo.listen(out.+=(_)) //IntelliJ highlight error
    List(
      SE('A', 'a', None),
      SE('B', 'b', None),
      SE('C', 'c', Some(eb)),
      SE('D', 'd', None),
      SE('E', 'e', Some(ea)),
      SE('F', 'f', None),
      SE('G', 'g', Some(eb)),
      SE('H', 'h', Some(ea)),
      SE('I', 'i', Some(ea))
    ).foreach(ese.send)
    l.unlisten()
    assertEquals(List('A', 'B', 'C', 'd', 'e', 'F', 'G', 'h', 'I'), out)
  }

  @Test
  def testSwitchSSimultaneous(): Unit = {
    val ss1 = SS2()
    val css = new BehaviorSink[SS2](ss1)
    val so = Behavior.switchS(css.map[Stream[Int]](_.s))
    val out = new ListBuffer[Int]()
    val l = so.listen(out.+=)
    val ss3 = SS2()
    val ss4 = SS2()
    val ss2 = SS2()
    List(0, 1, 2).foreach(ss1.s.send)
    css.send(ss2)
    ss1.s.send(7)
    List(3, 4).foreach(ss2.s.send)
    ss3.s.send(2)
    css.send(ss3)
    List(5, 6, 7).foreach(ss3.s.send)
    Transaction(_ => {
      ss3.s.send(8)
      css.send(ss4)
      ss4.s.send(2)
    })
    ss4.s.send(9)
    l.unlisten()
    assertEquals(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), out)
  }

  @Test
  def testLoopCell(): Unit = {
    val sa = new StreamSink[Int]()
    val sum_out = Transaction(_ => {
      val sum = new CellLoop[Int]()
      val sum_out_ = sa.snapshot[Int, Int](sum, (x: Int, y: Int) => x + y).hold(0)
      sum.loop(sum_out_)
      sum_out_
    })
    val out = new ListBuffer[Int]()
    val l = sum_out.listen(out.+=)
    List(2, 3, 1).foreach(sa.send)
    l.unlisten()
    assertEquals(List(0, 2, 5, 6), out)
    assertEquals(6.toLong, sum_out.sample().toLong)
  }

  @Test
  def testAccum(): Unit = {
    val sa = new StreamSink[Int]()
    val out = new ListBuffer[Int]()
    val sum = sa.accum[Int](100, (a, s) => a + s)
    val l = sum.listen(out.+=)
    List(5, 7, 1, 2, 3).foreach(sa.send)
    l.unlisten()
    assertEquals(List(100, 105, 112, 113, 115, 118), out)
  }

  @Test
  def testLoopValueSnapshot(): Unit = {
    val out = new ListBuffer[String]()
    val l: Listener = Transaction(_ => {
      val a = new Behavior("lettuce")
      val b = new BehaviorLoop[String]()
      val eSnap = Operational.value(a).snapshot[String, String](b, (aa: String, bb: String) => aa + " " + bb)
      b.loop(new Behavior[String]("cheese"))
      eSnap.listen(out.+=)
    })

    l.unlisten()
    assertEquals(List("lettuce cheese"), out)
  }

  @Test
  def testLoopValueHold(): Unit = {
    val out = new ListBuffer[String]()
    val value = Transaction(_ => {
      val a = new BehaviorLoop[String]()
      val value_ = Operational.value(a).hold("onion")
      a.loop(new Behavior[String]("cheese"))
      value_
    })
    val eTick = new StreamSink[Int]()
    val l = eTick.snapshot(value).listen(out.+=)
    eTick.send(0)
    l.unlisten()
    assertEquals(List("cheese"), out)
  }

  @Test
  def testLiftLoop(): Unit = {
    val out = new ListBuffer[String]()
    val b = new BehaviorSink("kettle")
    val c = Transaction(_ => {
      val a = new BehaviorLoop[String]()
      val c_ = a.lift[String, String](b, (aa, bb) => aa + " " + bb)
      a.loop(new Behavior[String]("tea"))
      c_
    })
    val l = c.listen(out.+=)
    b.send("caddy")
    l.unlisten()
    assertEquals(List("tea kettle", "tea caddy"), out)
  }

  @Test
  def testSwitchAndDefer(): Unit = {
    val out = new ListBuffer[String]()
    val si = new StreamSink[Int]()
    val l: Listener = Cell
      .switchS(
        si.map(i => {
            val c = new Behavior("A" + i)
            Operational.defer(Operational.value(c))
          })
          .hold(new Stream[String]()))
      .listen(out.+=)
    List(2, 4).foreach(si.send)
    l.unlisten()
    assertEquals(List("A2", "A4"), out)
  }

}

object BehaviorTester {

  case class SB(a: Option[Character], b: Option[Character], sw: Option[Cell[Character]])

  case class SE(a: Character, b: Character, sw: Option[Stream[Character]])

  case class SS2(s: StreamSink[Int] = new StreamSink[Int]())

}
