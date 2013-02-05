import sodium.*;

public class Clock
{
    public static Behavior<Double> mkClock()
    {
        long t0 = System.currentTimeMillis();
        final BehaviorSink<Double> clock = new BehaviorSink<Double>(0.0);
        new Thread() {
            public void run() {
                while (true) {
                    try { Thread.sleep(10); } catch (InterruptedException e) {}
                    long t = System.currentTimeMillis() - t0;
                    double td = (double)t * 0.001;
                    clock.send(td);
                }
            }
        }.start();
        return clock;
    }
}
