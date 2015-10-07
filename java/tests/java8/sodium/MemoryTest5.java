package nz.sodium;

public class MemoryTest5
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

        StreamSink<Integer> eChange = new StreamSink<Integer>();
        Cell<Integer> out = eChange.hold(0);
        Listener l = out.listen(tt -> {
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
