package sodium;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;

public class CellTester extends TestCase {
	@Override
	protected void tearDown() throws Exception {
		System.gc();
		Thread.sleep(100);
	}
	
	public void testHold()
    {
        StreamSink<Integer> e = new StreamSink<Integer>();
        Cell<Integer> b = e.hold(0);
        List<Integer> out = new ArrayList<Integer>();
        Listener l = b.updates().listen(x -> { out.add(x); });
        e.send(2);
        e.send(9);
        l.unlisten();
        assertEquals(Arrays.asList(2,9), out);
    }

	public void testSnapshot()
    {
        CellSink<Integer> b = new CellSink<Integer>(0);
        StreamSink<Long> trigger = new StreamSink<Long>();
        List<String> out = new ArrayList<String>();
        Listener l = trigger.snapshot(b, (x, y) -> x + " " + y)
            .listen(x -> { out.add(x); });
        trigger.send(100L);
        b.send(2);
        trigger.send(200L);
        b.send(9);
        b.send(1);
        trigger.send(300L);
        l.unlisten();
        assertEquals(Arrays.asList("100 0", "200 2", "300 1"), out);
    }
	
	public void testValues() {
		CellSink<Integer> b = new CellSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = b.value().listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9,2,7), out);
	}
	
	public void testConstantBehavior() {
	    Cell<Integer> b = new Cell<Integer>(12);
	    List<Integer> out = new ArrayList();
	    Listener l = b.value().listen(x -> { out.add(x); });
	    l.unlisten();
	    assertEquals(Arrays.asList(12), out);
	}

	public void testValuesThenMap() {
		CellSink<Integer> b = new CellSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = b.value().map(x -> x+100).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(109,102,107), out);
	}

	/**
	 * This is used for tests where value() produces a single initial value on listen,
	 * and then we double that up by causing that single initial event to be repeated.
	 * This needs testing separately, because the code must be done carefully to achieve
	 * this.
	 */
	private static Stream<Integer> doubleUp(Stream<Integer> ev)
	{
	    return ev.merge(ev);
	}

	public void testValuesTwiceThenMap() {
		CellSink<Integer> b = new CellSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = doubleUp(b.value()).map(x -> x+100).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(109,109,102,102,107,107), out);
	}

	public void testValuesThenCoalesce() {
		CellSink<Integer> b = new CellSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = b.value().coalesce((fst, snd) -> snd).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9,2,7), out);
	}

	public void testValuesTwiceThenCoalesce() {
		CellSink<Integer> b = new CellSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = doubleUp(b.value()).coalesce((fst, snd) -> fst+snd).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(18,4,14), out);
	}

	public void testValuesThenSnapshot() {
		CellSink<Integer> bi = new CellSink<Integer>(9);
		CellSink<Character> bc = new CellSink<Character>('a');
		List<Character> out = new ArrayList<Character>();
		Listener l = bi.value().snapshot(bc).listen(x -> { out.add(x); });
		bc.send('b');
		bi.send(2);
		bc.send('c');
		bi.send(7);
		l.unlisten();
		assertEquals(Arrays.asList('a','b','c'), out);
	}

	public void testValuesTwiceThenSnapshot() {
		CellSink<Integer> bi = new CellSink<Integer>(9);
		CellSink<Character> bc = new CellSink<Character>('a');
		List<Character> out = new ArrayList<Character>();
		Listener l = doubleUp(bi.value()).snapshot(bc).listen(x -> { out.add(x); });
		bc.send('b');
		bi.send(2);
		bc.send('c');
		bi.send(7);
		l.unlisten();
		assertEquals(Arrays.asList('a','a','b','b','c','c'), out);
	}

	public void testValuesThenMerge() {
		CellSink<Integer> bi = new CellSink<Integer>(9);
		CellSink<Integer> bj = new CellSink<Integer>(2);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = bi.value().merge(bj.value(), (x, y) -> x+y)
		    .listen(x -> { out.add(x); });
		bi.send(1);
		bj.send(4);
		l.unlisten();
		assertEquals(Arrays.asList(11,1,4), out);
	}

	public void testValuesThenFilter() {
		CellSink<Integer> b = new CellSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = b.value().filter(a -> true).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9,2,7), out);
	}

	public void testValuesTwiceThenFilter() {
		CellSink<Integer> b = new CellSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = doubleUp(b.value()).filter(a -> true).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9,9,2,2,7,7), out);
	}

	public void testValuesThenOnce() {
		CellSink<Integer> b = new CellSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = b.value().once().listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9), out);
	}

	public void testValuesTwiceThenOnce() {
		CellSink<Integer> b = new CellSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = doubleUp(b.value()).once().listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9), out);
	}

	public void testValuesLateListen() {
		CellSink<Integer> b = new CellSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Stream<Integer> value = b.value();
		b.send(8);
		Listener l = value.listen(x -> { out.add(x); });
		b.send(2);
		l.unlisten();
		assertEquals(Arrays.asList(8,2), out);
	}
	
	public void testMapB() {
		CellSink<Integer> b = new CellSink<Integer>(6);
		List<String> out = new ArrayList<String>();
		Listener l = b.map(x -> x.toString())
				.value().listen(x -> { out.add(x); });
		b.send(8);
		l.unlisten();
		assertEquals(Arrays.asList("6", "8"), out);
	}
	
	public void testMapBLateListen() {
		CellSink<Integer> b = new CellSink<Integer>(6);
		List<String> out = new ArrayList<String>();
		Cell<String> bm = b.map(x -> x.toString());
		b.send(2);
		Listener l = bm.value().listen(x -> { out.add(x); });
		b.send(8);
		l.unlisten();
		assertEquals(Arrays.asList("2", "8"), out);
	}
	
	public void testTransaction() {
		final boolean[] calledBack = new boolean[1];
	    Transaction.run((Transaction trans) -> {
	    	trans.prioritized(Node.NULL, trans2 -> { calledBack[0] = true; });
	    });
	    assertEquals(true, calledBack[0]);
	}

	public void testApply() {
		CellSink<Lambda1<Long, String>> bf = new CellSink<Lambda1<Long, String>>(
				(Long b) -> "1 "+b);
		CellSink<Long> ba = new CellSink<Long>(5L);
		List<String> out = new ArrayList<String>();
		Listener l = Cell.apply(bf,ba).value().listen(x -> { out.add(x); });
		bf.send((Long b) -> "12 "+b);
		ba.send(6L);
        l.unlisten();
        assertEquals(Arrays.asList("1 5", "12 5", "12 6"), out);
	}

	public void testLift() {
		CellSink<Integer> a = new CellSink<Integer>(1);
		CellSink<Long> b = new CellSink<Long>(5L);
		List<String> out = new ArrayList<String>();
		Listener l = Cell.lift(
			(x, y) -> x + " " + y,
			a,
			b
		).value().listen((String x) -> { out.add(x); });
		a.send(12);
		b.send(6L);
        l.unlisten();
        assertEquals(Arrays.asList("1 5", "12 5", "12 6"), out);
	}
	
	public void testLiftGlitch() {
		CellSink<Integer> a = new CellSink<Integer>(1);
		Cell<Integer> a3 = a.map((Integer x) -> x * 3);
		Cell<Integer> a5 = a.map((Integer x) -> x * 5);
		Cell<String> b = Cell.lift((x, y) -> x + " " + y, a3, a5);
		List<String> out = new ArrayList<String>();
		Listener l = b.value().listen((String x) -> { out.add(x); });
		a.send(2);
		l.unlisten();
		assertEquals(Arrays.asList("3 5", "6 10"), out);
	}

	public void testHoldIsDelayed() {
	    StreamSink<Integer> e = new StreamSink<Integer>();
	    Cell<Integer> h = e.hold(0);
	    Stream<String> pair = e.snapshot(h, (a, b) -> a + " " + b);
		List<String> out = new ArrayList<String>();
		Listener l = pair.listen((String x) -> { out.add(x); });
		e.send(2);
		e.send(3);
		l.unlisten();
		assertEquals(Arrays.asList("2 0", "3 2"), out);
	}

	static class SB
	{
	    SB(Character a, Character b, Cell<Character> sw)
	    {
	        this.a = a;
	        this.b = b;
	        this.sw = sw;
	    }
	    Character a;
	    Character b;
	    Cell<Character> sw;
	}

	public void testSwitchB()
	{
	    StreamSink<SB> esb = new StreamSink();
	    // Split each field out of SB so we can update multiple behaviours in a
	    // single transaction.
	    Cell<Character> ba = esb.map(s -> s.a).filterNotNull().hold('A');
	    Cell<Character> bb = esb.map(s -> s.b).filterNotNull().hold('a');
	    Cell<Cell<Character>> bsw = esb.map(s -> s.sw).filterNotNull().hold(ba);
	    Cell<Character> bo = Cell.switchC(bsw);
		List<Character> out = new ArrayList<Character>();
	    Listener l = bo.value().listen(c -> { out.add(c); });
	    esb.send(new SB('B','b',null));
	    esb.send(new SB('C','c',bb));
	    esb.send(new SB('D','d',null));
	    esb.send(new SB('E','e',ba));
	    esb.send(new SB('F','f',null));
	    esb.send(new SB(null,null,bb));
	    esb.send(new SB(null,null,ba));
	    esb.send(new SB('G','g',bb));
	    esb.send(new SB('H','h',ba));
	    esb.send(new SB('I','i',ba));
	    l.unlisten();
	    assertEquals(Arrays.asList('A','B','c','d','E','F','f','F','g','H','I'), out);
	}

	static class SE
	{
	    SE(Character a, Character b, Stream<Character> sw)
	    {
	        this.a = a;
	        this.b = b;
	        this.sw = sw;
	    }
	    Character a;
	    Character b;
	    Stream<Character> sw;
	}

    public void testSwitchE()
    {
        StreamSink<SE> ese = new StreamSink();
        Stream<Character> ea = ese.map(s -> s.a).filterNotNull();
        Stream<Character> eb = ese.map(s -> s.b).filterNotNull();
        Cell<Stream<Character>> bsw = ese.map(s -> s.sw).filterNotNull().hold(ea);
        List<Character> out = new ArrayList();
        Stream<Character> eo = Cell.switchS(bsw);
	    Listener l = eo.listen(c -> { out.add(c); });
	    ese.send(new SE('A','a',null));
	    ese.send(new SE('B','b',null));
	    ese.send(new SE('C','c',eb));
	    ese.send(new SE('D','d',null));
	    ese.send(new SE('E','e',ea));
	    ese.send(new SE('F','f',null));
	    ese.send(new SE('G','g',eb));
	    ese.send(new SE('H','h',ea));
	    ese.send(new SE('I','i',ea));
	    l.unlisten();
	    assertEquals(Arrays.asList('A','B','C','d','e','F','G','h','I'), out);
    }

    public void testLoopBehavior()
    {
        final StreamSink<Integer> ea = new StreamSink();
        Cell<Integer> sum_out = Transaction.<Cell<Integer>>run(() -> {
            CellLoop<Integer> sum = new CellLoop<Integer>();
            Cell<Integer> sum_out_ = ea.snapshot(sum, (x, y) -> x+y).hold(0);
            sum.loop(sum_out_);
            return sum_out_;
        });
        List<Integer> out = new ArrayList();
        Listener l = sum_out.value().listen(x -> { out.add(x); });
        ea.send(2);
        ea.send(3);
        ea.send(1);
        l.unlisten();
        assertEquals(Arrays.asList(0,2,5,6), out);
        assertEquals((int)6, (int)sum_out.sample());
    }

    public void testCollect()
    {
        StreamSink<Integer> ea = new StreamSink();
        List<Integer> out = new ArrayList();
        Cell<Integer> sum = ea.hold(100).collect(0,
            //(a,s) -> new Tuple2(a+s, a+s)
            new Lambda2<Integer, Integer, Tuple2<Integer,Integer>>() {
                public Tuple2<Integer,Integer> apply(Integer a, Integer s) {
                    return new Tuple2<Integer,Integer>(a+s, a+s);
                }
            }
        );
        Listener l = sum.value().listen((x) -> { out.add(x); });
        ea.send(5);
        ea.send(7);
        ea.send(1);
        ea.send(2);
        ea.send(3);
        l.unlisten();
        assertEquals(Arrays.asList(100,105,112,113,115,118), out);
    }

    public void testAccum()
    {
        StreamSink<Integer> ea = new StreamSink();
        List<Integer> out = new ArrayList();
        Cell<Integer> sum = ea.accum(100, (a,s)->a+s);
        Listener l = sum.value().listen((x) -> { out.add(x); });
        ea.send(5);
        ea.send(7);
        ea.send(1);
        ea.send(2);
        ea.send(3);
        l.unlisten();
        assertEquals(Arrays.asList(100,105,112,113,115,118), out);
    }

    public void testLoopValueSnapshot()
    {
        List<String> out = new ArrayList();
        Stream<String> eSnap = Transaction.<Stream<String>>run(() -> {
            Cell<String> a = new Cell("lettuce");
            CellLoop<String> b = new CellLoop();
            Stream<String> eSnap_ = a.value().snapshot(b, (String aa, String bb) -> aa + " " + bb);
            b.loop(new Cell<String>("cheese"));
            return eSnap_;
        });
        Listener l = eSnap.listen((x) -> { out.add(x); });
        l.unlisten();
        assertEquals(Arrays.asList("lettuce cheese"), out);
    }

    public void testLoopValueHold()
    {
        List<String> out = new ArrayList();
        Cell<String> value = Transaction.<Cell<String>>run(() -> {
            CellLoop<String> a = new CellLoop();
            Cell<String> value_ = a.value().hold("onion");
            a.loop(new Cell<String>("cheese"));
            return value_;
        });
        StreamSink<Unit> eTick = new StreamSink();
        Listener l = eTick.snapshot(value).listen((x) -> { out.add(x); });
        eTick.send(Unit.UNIT);
        l.unlisten();
        assertEquals(Arrays.asList("cheese"), out);
    }

    public void testLiftLoop()
    {
        List<String> out = new ArrayList();
        CellSink<String> b = new CellSink("kettle");
        Cell<String> c = Transaction.<Cell<String>>run(() -> {
            CellLoop<String> a = new CellLoop();
            Cell<String> c_ = Cell.lift(
                (aa, bb) -> aa + " " + bb,
                a, b);
            a.loop(new Cell<String>("tea"));
            return c_;
        });
        Listener l = c.value().listen((x) -> { out.add(x); });
        b.send("caddy");
        l.unlisten();
        assertEquals(Arrays.asList("tea kettle", "tea caddy"), out);
    }
}

