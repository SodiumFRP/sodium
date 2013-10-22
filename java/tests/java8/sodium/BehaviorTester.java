package sodium;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;
import junit.framework.TestSuite;

public class BehaviorTester extends TestCase {
	@Override
	protected void tearDown() throws Exception {
		System.gc();
		Thread.sleep(100);
	}
	
	public void testHold()
    {
        EventSink<Integer> e = new EventSink<Integer>();
        Behavior<Integer> b = e.hold(0);
        List<Integer> out = new ArrayList<Integer>();
        Listener l = b.updates().listen(x -> { out.add(x); });
        e.send(2);
        e.send(9);
        l.unlisten();
        assertEquals(Arrays.asList(2,9), out);
    }

	public void testSnapshot()
    {
        BehaviorSink<Integer> b = new BehaviorSink<Integer>(0);
        EventSink<Long> trigger = new EventSink<Long>();
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
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = b.value().listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9,2,7), out);
	}
	
	public void testConstantBehavior() {
	    Behavior<Integer> b = new Behavior<Integer>(12);
	    List<Integer> out = new ArrayList();
	    Listener l = b.value().listen(x -> { out.add(x); });
	    l.unlisten();
	    assertEquals(Arrays.asList(12), out);
	}

	public void testValuesThenMap() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
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
	private static Event<Integer> doubleUp(Event<Integer> ev)
	{
	    return Event.merge(ev, ev);
	}

	public void testValuesTwiceThenMap() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = doubleUp(b.value()).map(x -> x+100).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(109,109,102,102,107,107), out);
	}

	public void testValuesThenCoalesce() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = b.value().coalesce((fst, snd) -> snd).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9,2,7), out);
	}

	public void testValuesTwiceThenCoalesce() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = doubleUp(b.value()).coalesce((fst, snd) -> fst+snd).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(18,4,14), out);
	}

	public void testValuesThenSnapshot() {
		BehaviorSink<Integer> bi = new BehaviorSink<Integer>(9);
		BehaviorSink<Character> bc = new BehaviorSink<Character>('a');
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
		BehaviorSink<Integer> bi = new BehaviorSink<Integer>(9);
		BehaviorSink<Character> bc = new BehaviorSink<Character>('a');
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
		BehaviorSink<Integer> bi = new BehaviorSink<Integer>(9);
		BehaviorSink<Integer> bj = new BehaviorSink<Integer>(2);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = Event.mergeWith((x, y) -> x+y, bi.value(),bj.value())
		    .listen(x -> { out.add(x); });
		bi.send(1);
		bj.send(4);
		l.unlisten();
		assertEquals(Arrays.asList(11,1,4), out);
	}

	public void testValuesThenFilter() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = b.value().filter(a -> true).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9,2,7), out);
	}

	public void testValuesTwiceThenFilter() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = doubleUp(b.value()).filter(a -> true).listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9,9,2,2,7,7), out);
	}

	public void testValuesThenOnce() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = b.value().once().listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9), out);
	}

	public void testValuesTwiceThenOnce() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Listener l = doubleUp(b.value()).once().listen(x -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9), out);
	}

	public void testValuesLateListen() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Event<Integer> value = b.value();
		b.send(8);
		Listener l = value.listen(x -> { out.add(x); });
		b.send(2);
		l.unlisten();
		assertEquals(Arrays.asList(8,2), out);
	}
	
	public void testMapB() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(6);
		List<String> out = new ArrayList<String>();
		Listener l = b.map(x -> x.toString())
				.value().listen(x -> { out.add(x); });
		b.send(8);
		l.unlisten();
		assertEquals(Arrays.asList("6", "8"), out);
	}
	
	public void testMapBLateListen() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(6);
		List<String> out = new ArrayList<String>();
		Behavior<String> bm = b.map(x -> x.toString());
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
		BehaviorSink<Lambda1<Long, String>> bf = new BehaviorSink<Lambda1<Long, String>>(
				(Long b) -> "1 "+b);
		BehaviorSink<Long> ba = new BehaviorSink<Long>(5L);
		List<String> out = new ArrayList<String>();
		Listener l = Behavior.apply(bf,ba).value().listen(x -> { out.add(x); });
		bf.send((Long b) -> "12 "+b);
		ba.send(6L);
        l.unlisten();
        assertEquals(Arrays.asList("1 5", "12 5", "12 6"), out);
	}

	public void testLift() {
		BehaviorSink<Integer> a = new BehaviorSink<Integer>(1);
		BehaviorSink<Long> b = new BehaviorSink<Long>(5L);
		List<String> out = new ArrayList<String>();
		Listener l = Behavior.lift(
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
		BehaviorSink<Integer> a = new BehaviorSink<Integer>(1);
		Behavior<Integer> a3 = a.map((Integer x) -> x * 3);
		Behavior<Integer> a5 = a.map((Integer x) -> x * 5);
		Behavior<String> b = Behavior.lift((x, y) -> x + " " + y, a3, a5);
		List<String> out = new ArrayList<String>();
		Listener l = b.value().listen((String x) -> { out.add(x); });
		a.send(2);
		l.unlisten();
		assertEquals(Arrays.asList("3 5", "6 10"), out);
	}

	public void testHoldIsDelayed() {
	    EventSink<Integer> e = new EventSink<Integer>();
	    Behavior<Integer> h = e.hold(0);
	    Event<String> pair = e.snapshot(h, (a, b) -> a + " " + b);
		List<String> out = new ArrayList<String>();
		Listener l = pair.listen((String x) -> { out.add(x); });
		e.send(2);
		e.send(3);
		l.unlisten();
		assertEquals(Arrays.asList("2 0", "3 2"), out);
	}

	static class SB
	{
	    SB(Character a, Character b, Behavior<Character> sw)
	    {
	        this.a = a;
	        this.b = b;
	        this.sw = sw;
	    }
	    Character a;
	    Character b;
	    Behavior<Character> sw;
	}

	public void testSwitchB()
	{
	    EventSink<SB> esb = new EventSink();
	    // Split each field out of SB so we can update multiple behaviours in a
	    // single transaction.
	    Behavior<Character> ba = esb.map(s -> s.a).filterNotNull().hold('A');
	    Behavior<Character> bb = esb.map(s -> s.b).filterNotNull().hold('a');
	    Behavior<Behavior<Character>> bsw = esb.map(s -> s.sw).filterNotNull().hold(ba);
	    Behavior<Character> bo = Behavior.switchB(bsw);
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
	    SE(Character a, Character b, Event<Character> sw)
	    {
	        this.a = a;
	        this.b = b;
	        this.sw = sw;
	    }
	    Character a;
	    Character b;
	    Event<Character> sw;
	}

    public void testSwitchE()
    {
        EventSink<SE> ese = new EventSink();
        Event<Character> ea = ese.map(s -> s.a).filterNotNull();
        Event<Character> eb = ese.map(s -> s.b).filterNotNull();
        Behavior<Event<Character>> bsw = ese.map(s -> s.sw).filterNotNull().hold(ea);
        List<Character> out = new ArrayList();
        Event<Character> eo = Behavior.switchE(bsw);
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
        final EventSink<Integer> ea = new EventSink();
        BehaviorLoop<Integer> sum = new BehaviorLoop<Integer>();
        Behavior<Integer> sum_out = ea.snapshot(sum, (x, y) -> x+y).hold(0);
        sum.loop(sum_out);
        List<Integer> out = new ArrayList();
        Listener l = sum_out.value().listen(x -> { out.add(x); });
        ea.send(2);
        ea.send(3);
        ea.send(1);
        l.unlisten();
        assertEquals(Arrays.asList(0,2,5,6), out);
        assertEquals((int)6, (int)sum.sample());
    }

    public void testCollect()
    {
        EventSink<Integer> ea = new EventSink();
        List<Integer> out = new ArrayList();
        Behavior<Integer> sum = ea.hold(100).collect(0,
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
        EventSink<Integer> ea = new EventSink();
        List<Integer> out = new ArrayList();
        Behavior<Integer> sum = ea.accum(100, (a,s)->a+s);
        Listener l = sum.value().listen((x) -> { out.add(x); });
        ea.send(5);
        ea.send(7);
        ea.send(1);
        ea.send(2);
        ea.send(3);
        l.unlisten();
        assertEquals(Arrays.asList(100,105,112,113,115,118), out);
    }
}

