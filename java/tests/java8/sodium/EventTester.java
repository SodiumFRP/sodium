package sodium;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;

public class EventTester extends TestCase {
	@Override
	protected void tearDown() throws Exception {
		System.gc();
		Thread.sleep(100);
	}

	public void testSendEvent()
    {
        EventSink<Integer> e = new EventSink();
        List<Integer> out = new ArrayList();
        Listener l = e.listen(x -> { out.add(x); });
        e.send(5);
        l.unlisten();
        assertEquals(Arrays.asList(5), out);
        e.send(6);
        assertEquals(Arrays.asList(5), out);
    }

	public void testMap()
    {
        EventSink<Integer> e = new EventSink();
        Event<String> m = e.map(x -> Integer.toString(x));
        List<String> out = new ArrayList();
        Listener l = m.listen((String x) -> { out.add(x); });
        e.send(5);
        l.unlisten();
        assertEquals(Arrays.asList("5"), out);
    }

    public void testMergeNonSimultaneous()
    {
        EventSink<Integer> e1 = new EventSink();
        EventSink<Integer> e2 = new EventSink();
        List<Integer> out = new ArrayList();
        Listener l = Event.merge(e1,e2).listen(x -> { out.add(x); });
        e1.send(7);
        e2.send(9);
        e1.send(8);
        l.unlisten();
        assertEquals(Arrays.asList(7,9,8), out);
    }

    public void testMergeSimultaneous()
    {
        EventSink<Integer> e = new EventSink();
        List<Integer> out = new ArrayList();
        Listener l = Event.merge(e,e).listen(x -> { out.add(x); });
        e.send(7);
        e.send(9);
        l.unlisten();
        assertEquals(Arrays.asList(7,7,9,9), out);
    }

    public void testCoalesce()
    {
        EventSink<Integer> e1 = new EventSink();
        EventSink<Integer> e2 = new EventSink();
        List<Integer> out = new ArrayList();
        Listener l =
             Event.merge(e1,Event.merge(e1.map(x -> x * 100), e2))
            .coalesce((Integer a, Integer b) -> a+b)
            .listen((Integer x) -> { out.add(x); });
        e1.send(2);
        e1.send(8);
        e2.send(40);
        l.unlisten();
        assertEquals(Arrays.asList(202, 808, 40), out);
    }
    
    public void testFilter()
    {
        EventSink<Character> e = new EventSink();
        List<Character> out = new ArrayList();
        Listener l = e.filter((Character c) -> Character.isUpperCase(c)).listen((Character c) -> { out.add(c); });
        e.send('H');
        e.send('o');
        e.send('I');
        l.unlisten();
        assertEquals(Arrays.asList('H','I'), out);
    }

    public void testFilterNotNull()
    {
        EventSink<String> e = new EventSink();
        List<String> out = new ArrayList();
        Listener l = e.filterNotNull().listen(s -> { out.add(s); });
        e.send("tomato");
        e.send(null);
        e.send("peach");
        l.unlisten();
        assertEquals(Arrays.asList("tomato","peach"), out);
    }

    public void testLoopEvent()
    {
        final EventSink<Integer> ea = new EventSink();
        EventLoop<Integer> eb = new EventLoop<Integer>();
        Event<Integer> ec = Event.mergeWith((x, y) -> x+y, ea.map(x -> x % 10), eb);
        Event<Integer> eb_out = ea.map(x -> x / 10).filter(x -> x != 0);
        eb.loop(eb_out);
        List<Integer> out = new ArrayList();
        Listener l = ec.listen(x -> { out.add(x); });
        ea.send(2);
        ea.send(52);
        l.unlisten();
        assertEquals(Arrays.asList(2,7), out);
    }

    public void testGate()
    {
        EventSink<Character> ec = new EventSink();
        BehaviorSink<Boolean> epred = new BehaviorSink(true);
        List<Character> out = new ArrayList();
        Listener l = ec.gate(epred).listen(x -> { out.add(x); });
        ec.send('H');
        epred.send(false);
        ec.send('O');
        epred.send(true);
        ec.send('I');
        l.unlisten();
        assertEquals(Arrays.asList('H','I'), out);
    }

    public void testCollect()
    {
        EventSink<Integer> ea = new EventSink();
        List<Integer> out = new ArrayList();
        Event<Integer> sum = ea.collect(100,
            //(a,s) -> new Tuple2(a+s, a+s)
            new Lambda2<Integer, Integer, Tuple2<Integer,Integer>>() {
                public Tuple2<Integer,Integer> apply(Integer a, Integer s) {
                    return new Tuple2<Integer,Integer>(a+s, a+s);
                }
            }
        );
        Listener l = sum.listen((x) -> { out.add(x); });
        ea.send(5);
        ea.send(7);
        ea.send(1);
        ea.send(2);
        ea.send(3);
        l.unlisten();
        assertEquals(Arrays.asList(105,112,113,115,118), out);
    }

    public void testAccum()
    {
        EventSink<Integer> ea = new EventSink();
        List<Integer> out = new ArrayList();
        Behavior<Integer> sum = ea.accum(100, (a,s)->a+s);
        Listener l = sum.updates().listen((x) -> { out.add(x); });
        ea.send(5);
        ea.send(7);
        ea.send(1);
        ea.send(2);
        ea.send(3);
        l.unlisten();
        assertEquals(Arrays.asList(105,112,113,115,118), out);
    }

    public void testOnce()
    {
        EventSink<Character> e = new EventSink();
        List<Character> out = new ArrayList();
        Listener l = e.once().listen((x) -> { out.add(x); });
        e.send('A');
        e.send('B');
        e.send('C');
        l.unlisten();
        assertEquals(Arrays.asList('A'), out);
    }

    public void testDelay()
    {
        EventSink<Character> e = new EventSink();
        Behavior<Character> b = e.hold(' ');
        List<Character> out = new ArrayList();
        Listener l = e.delay().snapshot(b).listen((x) -> { out.add(x); });
        e.send('C');
        e.send('B');
        e.send('A');
        l.unlisten();
        assertEquals(Arrays.asList('C','B','A'), out);
    }
}

