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
        Listener l = e.listen((Integer x) -> { out.add(x); });
        e.send(5);
        l.unlisten();
        assertEquals(Arrays.asList(5), out);
        e.send(6);
        assertEquals(Arrays.asList(5), out);
    }

	public void testMap()
    {
        EventSink<Integer> e = new EventSink();
        Event<String> m = e.map((Integer x) -> Integer.toString(x));
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
        Listener l = Event.merge(e1,e2).listen((Integer x) -> { out.add(x); });
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
        Listener l = Event.merge(e,e).listen((Integer x) -> { out.add(x); });
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
             Event.merge(e1,Event.merge(e1.map((Integer x) -> x * 100), e2))
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
        Listener l = e.filterNotNull().listen((String s) -> { out.add(s); });
        e.send("tomato");
        e.send(null);
        e.send("peach");
        l.unlisten();
        assertEquals(Arrays.asList("tomato","peach"), out);
    }
}

