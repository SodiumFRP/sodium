package nz.sodium.time;

import java.util.TreeSet;

class MillisecondsTimerSystemImpl implements TimerSystemImpl<Long> {
    public class SimpleTimer implements Timer, Comparable<SimpleTimer>  {
        private SimpleTimer(long t, Runnable callback) {
            this.t = t;
            synchronized (lock) {
                this.seq = nextSeq++;
            }
            this.callback = callback;
        }
        private long t;
        private long seq;
        private Runnable callback;
        public void cancel() {
            synchronized (lock) {
                timers.remove(this);
            }
        }
		@Override
		public int compareTo(SimpleTimer o) {
		    if (t < o.t) return -1;
		    if (t > o.t) return 1;
		    if (seq < o.seq) return -1;
		    if (seq > o.seq) return 1;
		    return 0;
		}
    }
    private Object lock = new Object();
    private long nextSeq = 0;
    private TreeSet<SimpleTimer> timers = new TreeSet<SimpleTimer>();

    private long timeTillNext(long now) {
        while (true) {
            SimpleTimer fired = null;
            long tWait;
            synchronized (lock) {
                if (timers.isEmpty())
                    tWait = 1000000;
                else {
                    // How long till the first timer?
                    SimpleTimer timer = timers.first();
                    tWait = timer.t - now;
                    if (tWait <= 0) {
                        tWait = 0;
                        fired = timer;
                        timers.remove(fired);
                    }
                }
            }
            if (fired != null)
                fired.callback.run();
            else
                return tWait;
        }
    }

    private Thread timerThread = new Thread() {
        public void run() {
            while (true) {
                long tWait = timeTillNext(System.currentTimeMillis());
                if (tWait > 0) {
                    try {
                        Thread.sleep(tWait);
                    }
                    catch (InterruptedException e) {
                    }
                }
            }
        }
    };
    public MillisecondsTimerSystemImpl() {
        timerThread.setDaemon(true);
        timerThread.start();
    }
    public Timer setTimer(Long t, Runnable callback) {
        SimpleTimer timer = new SimpleTimer(t, callback);
        synchronized (lock) {
            timers.add(timer);
            timerThread.interrupt();
        }
        return timer;
    }
    public void runTimersTo(Long now) {
        timeTillNext(now);
    }
    public Long now() {
        return System.currentTimeMillis();
    }
}

