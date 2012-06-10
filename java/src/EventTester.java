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
        EventSink<Integer> e = new EventSink<Integer>();
        List<Integer> out = new ArrayList<Integer>();
        Listener l = e.listen((Integer x) -> { out.add(x); });
        e.send(5);
        l.unlisten();
        assertEquals(Arrays.asList(5), out);
        e.send(6);
        assertEquals(Arrays.asList(5), out);
    }

	public void testMap()
    {
        EventSink<Integer> e = new EventSink<Integer>();
        Event<String> m = e.map((Integer x) -> Integer.toString(x));
        List<String> out = new ArrayList<String>();
        Listener l = m.listen((String x) -> { out.add(x); });
        e.send(5);
        l.unlisten();
        assertEquals(Arrays.asList("5"), out);
    }
}
