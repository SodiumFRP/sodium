package sodium;

import sodium.*;

public class MemoryTest1
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
        Event<Integer> etens = et.map(x -> x/10);
        Event<Integer> changeTens = et.snapshot(t, (neu, old) ->
            neu.equals(old) ? null : neu).filterNotNull();
        Behavior<Behavior<Tuple2<Integer,Integer>>> oout =
            changeTens.map(tens -> t.map(tt -> new Tuple2<Integer,Integer>(tens, tt))).
            hold(t.map(tt -> new Tuple2<Integer,Integer>(0, tt)));
        Behavior<Tuple2<Integer,Integer>> out = Behavior.switchB(oout);
        Listener l = out.value().listen(tu -> {
            //System.out.println(tu.a+","+tu.b);
        });
        int i = 0;
        while (i < 1000000000) {
            et.send(i);
            i++;
        }
        l.unlisten();
    }
}
