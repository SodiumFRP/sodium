package sodium;

import sodium.*;

public class MemoryTest4
{
    public static void main(String[] args)
    {
        new Thread() {
            public void run()
            {
                try {
                    while (true) {
                        System.out.println("memory "+Runtime.getRuntime().totalMemory());
                        Thread.sleep(5000);
                    }
                }
                catch (InterruptedException e) {
                    System.out.println(e.toString());
                }
            }
        }.start();

        EventSink<Integer> et = new EventSink<Integer>();
        EventSink<Integer> eChange = new EventSink<Integer>();
        Behavior<Event<Integer>> oout = eChange.map(x -> (Event<Integer>)et).hold((Event<Integer>)et);
        Event<Integer> out = Behavior.switchE(oout);
        Listener l = out.listen(tt -> {
            System.out.println(tt);
        });
        int i = 0;
        while (i < 1000000000) {
            eChange.send(i);
            i++;
        }
        l.unlisten();
    }
}
