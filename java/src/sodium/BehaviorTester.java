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
        Listener l = b.changes().listen((Integer x) -> { out.add(x); });
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
        Listener l = trigger.snapshot(b, (Long x, Integer y) -> x + " " + y)
            .listen((String x) -> { out.add(x); });
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
		Listener l = b.values().listen((Integer x) -> { out.add(x); });
		b.send(2);
		b.send(7);
		l.unlisten();
		assertEquals(Arrays.asList(9,2,7), out);
	}

	public void testValuesLateListen() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(9);
		List<Integer> out = new ArrayList<Integer>();
		Event<Integer> values = b.values();
		b.send(8);
		Listener l = values.listen((Integer x) -> { out.add(x); });
		b.send(2);
		l.unlisten();
		assertEquals(Arrays.asList(8,2), out);
	}
	
	public void testMapB() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(6);
		List<String> out = new ArrayList<String>();
		Listener l = b.map((Integer x) -> x.toString())
				.values().listen((String x) -> { out.add(x); });
		b.send(8);
		l.unlisten();
		assertEquals(Arrays.asList("6", "8"), out);
	}
	
	public void testMapBLateListen() {
		BehaviorSink<Integer> b = new BehaviorSink<Integer>(6);
		List<String> out = new ArrayList<String>();
		Behavior<String> bm = b.map((Integer x) -> x.toString());
		b.send(2);
		Listener l = bm.values().listen((String x) -> { out.add(x); });
		b.send(8);
		l.unlisten();
		assertEquals(Arrays.asList("2", "8"), out);
	}
	
	public void testTransaction() {
		final boolean[] calledBack = new boolean[1];
	    Transaction.run((Transaction trans) -> {
	    	trans.prioritized(Node.NULL, (Transaction trans2) -> { calledBack[0] = true; });
	    });
	    assertEquals(true, calledBack[0]);
	}

	public void testApply() {
		BehaviorSink<Lambda1<Long, String>> bf = new BehaviorSink<Lambda1<Long, String>>(
				(Long b) -> "1 "+b);
		BehaviorSink<Long> ba = new BehaviorSink<Long>(5L);
		List<String> out = new ArrayList<String>();
		Listener l = Behavior.apply(bf,ba).values().listen((String x) -> { out.add(x); });
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
			(Integer x, Long y) -> x + " " + y,
			a,
			b
		).values().listen((String x) -> { out.add(x); });
		a.send(12);
		b.send(6L);
        l.unlisten();
        assertEquals(Arrays.asList("1 5", "12 5", "12 6"), out);
	}
	
	public void testLiftGlitch() {
		BehaviorSink<Integer> a = new BehaviorSink<Integer>(1);
		Behavior<Integer> a3 = a.map((Integer x) -> x * 3);
		Behavior<Integer> a5 = a.map((Integer x) -> x * 5);
		Behavior<String> b = Behavior.lift((Integer x, Integer y) -> x + " " + y, a3, a5);
		List<String> out = new ArrayList<String>();
		Listener l = b.values().listen((String x) -> { out.add(x); });
		a.send(2);
		l.unlisten();
		assertEquals(Arrays.asList("3 5", "6 10"), out);
	}

	public void testHoldIsDelayed() {
	    EventSink<Integer> e = new EventSink<Integer>();
	    Behavior<Integer> h = e.hold(0);
	    Event<String> pair = e.snapshot(h, (Integer a, Integer b) -> a + " " + b);
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
	    Behavior<Character> ba = esb.map((SB s) -> s.a).filterNotNull().hold('A');
	    Behavior<Character> bb = esb.map((SB s) -> s.b).filterNotNull().hold('a');
	    Behavior<Behavior<Character>> bsw = esb.map((SB s) -> s.sw).filterNotNull().hold(ba);
	    Behavior<Character> bo = Behavior.switchB(bsw);
		List<Character> out = new ArrayList<Character>();
	    Listener l = bo.values().listen((Character c) -> { out.add(c); });
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
}

