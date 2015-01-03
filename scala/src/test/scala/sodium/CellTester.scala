package sodium

import junit.framework.TestCase
import junit.framework.Assert._

import java.util.Arrays
import java.util.ArrayList

 class CellTester extends TestCase {
  import CellTester._
  
	protected override def tearDown() {
		System.gc()
		Thread.sleep(100)
	}
	
	 def testHold()
    {
        val e = new StreamSink[Integer]()
        val b = e.hold(0)
        val out = new ArrayList[Integer]()
        val l = b.updates().listen(x => { out.add(x) })
        List(2,9).foreach(e.send(_))
        l.unlisten()
        assertEquals(Arrays.asList(2,9), out)
    }

	 def testSnapshot()
    {
        val b = new CellSink[Integer](0)
        val trigger = new StreamSink[Long]()
        val out = new ArrayList[String]()
        val l = trigger.snapshot(b, (x, y) => x + " " + y)
            .listen(x => { out.add(x) })
        trigger.send(100L)
        b.send(2)
        trigger.send(200L)
        b.send(9)
        b.send(1)
        trigger.send(300L)
        l.unlisten()
        assertEquals(Arrays.asList("100 0", "200 2", "300 1"), out)
    }
	
	 def testValues() {
		val b = new CellSink[Integer](9)
		val out = new CellSink[Integer]()
		val l = b.value().listen(x => { out.add(x) })
		List(2,7).foreach(b.send(_))
		l.unlisten()
		assertEquals(Arrays.asList(9,2,7), out)
	}
	
	 def testConstantBehavior() {
	    val b = new Cell[Integer](12)
	    val out = new ArrayList()
	    val l = b.value().listen(x -> { out.add(x) })
	    l.unlisten()
	    assertEquals(Arrays.asList(12), out)
	}

	 def testValuesThenMap() {
		val b = new CellSink[Integer](9)
		val out = new CellSink[Integer]()
		val l = b.value().map(x => x+100).listen(x => out.add(x))
		List(2,7).foreach(b.send(_))
		l.unlisten()
		assertEquals(Arrays.asList(109,102,107), out)
	}

	 def testValuesTwiceThenMap() {
		val b = new CellSink[Integer](9)
		val out = new CellSink[Integer]()
		val l = doubleUp(b.value()).map(x => x+100).listen(x => out.add(x))
		List(2,7).foreach(b.send(_))
		l.unlisten()
		assertEquals(Arrays.asList(109,109,102,102,107,107), out)
	}

	 def testValuesThenCoalesce() {
		val b = new CellSink[Integer](9)
		val out = new CellSink[Integer]()
		val l = b.value().coalesce((fst, snd) => snd).listen(x => out.add(x))
		List(2,7).foreach(b.send(_))
		l.unlisten()
		assertEquals(Arrays.asList(9,2,7), out)
	}

	 def testValuesTwiceThenCoalesce() {
		val b = new CellSink[Integer](9)
		val out = new CellSink[Integer]()
		val l = doubleUp(b.value()).coalesce((fst, snd) => fst+snd).listen(x => out.add(x))
		List(2,7).foreach(b.send(_))
		l.unlisten()
		assertEquals(Arrays.asList(18,4,14), out)
	}

	 def testValuesThenSnapshot() {
		val bi = new CellSink[Integer](9)
		val bc = new CellSink[Character]('a')
		val out = new ArrayList[Character]()
		val l = bi.value().snapshot(bc).listen(x -> out.add(x))
		bc.send('b')
		bi.send(2)
		bc.send('c')
		bi.send(7)
		l.unlisten()
		assertEquals(Arrays.asList('a','b','c'), out)
	}

	 def testValuesTwiceThenSnapshot() {
		val bi = new CellSink[Integer](9)
		val bc = new CellSink[Character]('a')
		val out = new ArrayList[Character]()
		val l = doubleUp(bi.value()).snapshot(bc).listen(x -> out.add(x))
		bc.send('b')
		bi.send(2)
		bc.send('c')
		bi.send(7)
		l.unlisten()
		assertEquals(Arrays.asList('a','a','b','b','c','c'), out)
	}

	 def testValuesThenMerge() {
		val bi = new CellSink[Integer](9)
		val bj = new CellSink[Integer](2)
		val out = new CellSink[Integer]()
		val l = bi.value().merge(bj.value(), (x, y) => x+y)
		    .listen(x => out.add(x))
		bi.send(1)
		bj.send(4)
		l.unlisten()
		assertEquals(Arrays.asList(11,1,4), out)
	}

	 def testValuesThenFilter() {
		val b = new CellSink[Integer](9)
		val out = new CellSink[Integer]()
		val l = b.value().filter(a => true).listen(x => out.add(x))
				List(2,7).foreach(b.send(_))
		l.unlisten()
		assertEquals(Arrays.asList(9,2,7), out)
	}

	 def testValuesTwiceThenFilter() {
		val b = new CellSink[Integer](9)
		val out = new CellSink[Integer]()
		val l = doubleUp(b.value()).filter(a => true).listen(x => out.add(x))
		List(2,7).foreach(b.send(_))
		l.unlisten()
		assertEquals(Arrays.asList(9,9,2,2,7,7), out)
	}

	 def testValuesThenOnce() {
		val b = new CellSink[Integer](9)
		val out = new CellSink[Integer]()
		val l = b.value().once().listen(x => { out.add(x) })
		List(2,7).foreach(b.send(_))
		l.unlisten()
		assertEquals(Arrays.asList(9), out)
	}

	 def testValuesTwiceThenOnce() {
		val b = new CellSink[Integer](9)
		val out = new CellSink[Integer]()
		val l = doubleUp(b.value()).once().listen(x => { out.add(x) })
		List(2,7).foreach(b.send(_))
		l.unlisten()
		assertEquals(Arrays.asList(9), out)
	}

	 def testValuesLateListen() {
		val b = new CellSink[Integer](9)
		val out = new CellSink[Integer]()
		val value = b.value()
		b.send(8)
		val l = value.listen(x => { out.add(x) })
		b.send(2)
		l.unlisten()
		assertEquals(Arrays.asList(8,2), out)
	}
	
	 def testMapB() {
		val b = new CellSink[Integer](6)
		val out = new ArrayList[String]()
		val l = b.map(x => x.toString())
				.value().listen(x => { out.add(x) })
		b.send(8)
		l.unlisten()
		assertEquals(Arrays.asList("6", "8"), out)
	}
	
	 def testMapBLateListen() {
		val b = new CellSink[Integer](6)
		val out = new ArrayList[String]()
		val bm = b.map(x => x.toString())
		b.send(2)
		val l = bm.value().listen(x => { out.add(x) })
		b.send(8)
		l.unlisten()
		assertEquals(Arrays.asList("2", "8"), out)
	}
	
	 def testTransaction() {
		val calledBack = new Array[Boolean](1)
	    Transaction.run((trans: Transaction) => {
	    	trans.prioritized(Node.NULL, trans2 => { calledBack(0) = true })
	    })
	    assertEquals(true, calledBack(0))
	}

	 def testApply() {
		val bf = new CellSink[Long => String](b => "1 "+b)
		val ba = new CellSink[Long](5L)
		val out = new ArrayList[String]()
		val l = Cell.apply(bf,ba).value().listen(x => { out.add(x) })
		bf.send(b => "12 "+b)
		ba.send(6L)
        l.unlisten()
        assertEquals(Arrays.asList("1 5", "12 5", "12 6"), out)
	}

	 def testLift() {
		val a = new CellSink[Integer](1)
		val b = new CellSink[Long](5L)
		val out = new ArrayList[String]()
		val l = Cell.lift(
			(x, y) => x + " " + y,
			a,
			b
		).value().listen(x => { out.add(x) })
		a.send(12)
		b.send(6L)
        l.unlisten()
        assertEquals(Arrays.asList("1 5", "12 5", "12 6"), out)
	}
	
	 def testLiftGlitch() {
		val a = new CellSink[Integer](1)
		val a3 = a.map(x => x * 3)
		val a5 = a.map(x => x * 5)
		val b = Cell.lift((x, y) => x + " " + y, a3, a5)
		val out = new ArrayList[String]()
		val l = b.value().listen(x => { out.add(x) })
		a.send(2)
		l.unlisten()
		assertEquals(Arrays.asList("3 5", "6 10"), out)
	}

	 def testHoldIsDelayed() {
	    val e = new StreamSink[Integer]()
	    val h = e.hold(0)
	    val pair = e.snapshot(h, (a, b) => a + " " + b)
		val out = new ArrayList[String]()
		val l = pair.listen((String x) -> { out.add(x) })
		e.send(2)
		e.send(3)
		l.unlisten()
		assertEquals(Arrays.asList("2 0", "3 2"), out)
	}

	 def testSwitchB()
	{
	    val esb = new StreamSink()
	    // Split each field out of SB so we can update multiple behaviours in a
	    // single transaction.
	    val ba = esb.map(s => s.a).filterNotNull().hold('A')
	    val bb = esb.map(s => s.b).filterNotNull().hold('a')
	    val bsw = esb.map(s => s.sw).filterNotNull().hold(ba)
	    val bo = Cell.switchC(bsw)
		val out = new ArrayList[Character]()
	    val l = bo.value().listen(c -> { out.add(c) })
	    esb.send(new SB('B','b',null))
	    esb.send(new SB('C','c',bb))
	    esb.send(new SB('D','d',null))
	    esb.send(new SB('E','e',ba))
	    esb.send(new SB('F','f',null))
	    esb.send(new SB(null,null,bb))
	    esb.send(new SB(null,null,ba))
	    esb.send(new SB('G','g',bb))
	    esb.send(new SB('H','h',ba))
	    esb.send(new SB('I','i',ba))
	    l.unlisten()
	    assertEquals(Arrays.asList('A','B','c','d','E','F','f','F','g','H','I'), out)
	}

     def testSwitchE()
    {
        val ese = new StreamSink()
        val ea = ese.map(s => s.a).filterNotNull()
        val eb = ese.map(s => s.b).filterNotNull()
        val bsw = ese.map(s => s.sw).filterNotNull().hold(ea)
        val out = new ArrayList()
        val eo = Cell.switchS(bsw)
	    val l = eo.listen(c -> { out.add(c) })
	    ese.send(new SE('A','a',null))
	    ese.send(new SE('B','b',null))
	    ese.send(new SE('C','c',eb))
	    ese.send(new SE('D','d',null))
	    ese.send(new SE('E','e',ea))
	    ese.send(new SE('F','f',null))
	    ese.send(new SE('G','g',eb))
	    ese.send(new SE('H','h',ea))
	    ese.send(new SE('I','i',ea))
	    l.unlisten()
	    assertEquals(Arrays.asList('A','B','C','d','e','F','G','h','I'), out)
    }

     def testLoopBehavior()
    {
        val ea = new StreamSink()
        val sum_out = Transaction.run[Cell[Integer]](() => {
            val sum = new CellLoop[Integer]()
            val sum_out_ = ea.snapshot(sum, (x, y) => x+y).hold(0)
            sum.loop(sum_out_)
            sum_out_
        })
        val out = new ArrayList()
        val l = sum_out.value().listen(x => { out.add(x) })
        ea.send(2)
        ea.send(3)
        ea.send(1)
        l.unlisten()
        assertEquals(Arrays.asList(0,2,5,6), out)
        assertEquals(6, sum_out.sample())
    }

     def testCollect()
    {
        val ea = new StreamSink()
        val out = new ArrayList()
        val sum = ea.hold(100).collect(0,
            (a, s) => (a+s, a+s)
        )
        val l = sum.value().listen((x) -> { out.add(x) })
        ea.send(5)
        ea.send(7)
        ea.send(1)
        ea.send(2)
        ea.send(3)
        l.unlisten()
        assertEquals(Arrays.asList(100,105,112,113,115,118), out)
    }

     def testAccum()
    {
        val ea = new StreamSink[Integer]()
        val out = new ArrayList()
        val sum = ea.accum(100, (a,s) => a+s)
        val l = sum.value().listen((x) => { out.add(x) })
        ea.send(5)
        ea.send(7)
        ea.send(1)
        ea.send(2)
        ea.send(3)
        l.unlisten()
        assertEquals(Arrays.asList(100,105,112,113,115,118), out)
    }

     def testLoopValueSnapshot()
    {
        val out = new ArrayList()
        val eSnap = Transaction.run[Stream[String]](() => {
            val a = new Cell("lettuce")
            val b = new CellLoop()
            val eSnap_ = a.value().snapshot(b, (aa, bb) => aa + " " + bb)
            b.loop(new Cell[String]("cheese"))
            return eSnap_
        })
        val l = eSnap.listen((x) -> { out.add(x) })
        l.unlisten()
        assertEquals(Arrays.asList("lettuce cheese"), out)
    }

     def testLoopValueHold()
    {
        val out = new ArrayList()
        val value = Transaction.run[Cell[String]](() => {
            val a = new CellLoop()
            val value_ = a.value().hold("onion")
            a.loop(new Cell[String]("cheese"))
            return value_
        })
        val eTick = new StreamSink()
        val l = eTick.snapshot(value).listen(x => { out.add(x) })
        eTick.send(Unit.UNIT)
        l.unlisten()
        assertEquals(Arrays.asList("cheese"), out)
    }

     def testLiftLoop()
    {
        val out = new ArrayList()
        val b = new CellSink("kettle")
        val c = Transaction.run[Cell[String]](() => {
            val a = new CellLoop()
            val c_ = Cell.lift(
                (aa, bb) => aa + " " + bb,
                a, b)
            a.loop(new Cell[String]("tea"))
            return c_
        })
        val l = c.value().listen(x => { out.add(x) })
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
	private def doubleUp(ev: Stream[Integer]): Stream[Integer] =
	    ev.merge(ev)
	    
	case class SB(val a: Character, val b: Character, val sw: Cell[Character])

	case class SE(val a: Character, val b: Character, val sw: Stream[Character])
}

