package sodium;

import sodium.*;

public class MemoryTest3
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
        Behavior<Integer> t = et.hold(0);
        EventSink<Integer> eChange = new EventSink<Integer>();
        Behavior<Behavior<Integer>> oout = eChange.map(x -> t).hold(t);
        Behavior<Integer> out = Behavior.switchB(oout);
        Listener l = out.value().listen(tt -> {
            //System.out.println(tt)
        });
        int i = 0;
        while (i < 1000000000) {
            eChange.send(i);
            i++;
        }
        l.unlisten();
    }
}
