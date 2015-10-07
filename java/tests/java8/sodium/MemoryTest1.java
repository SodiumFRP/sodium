package nz.sodium;

import java.util.Optional;

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

        StreamSink<Integer> et = new StreamSink<Integer>();
        Cell<Integer> t = et.hold(0);
        Stream<Integer> etens = et.map(x -> x/10);
        Stream<Integer> changeTens = Stream.filterOptional(et.snapshot(t, (neu, old) ->
            neu.equals(old) ? Optional.empty() : Optional.of(neu)));
        Cell<Cell<Tuple2<Integer,Integer>>> oout =
            changeTens.map(tens -> t.map(tt -> new Tuple2<Integer,Integer>(tens, tt))).
            hold(t.map(tt -> new Tuple2<Integer,Integer>(0, tt)));
        Cell<Tuple2<Integer,Integer>> out = Cell.switchC(oout);
        Listener l = out.listen(tu -> {
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
