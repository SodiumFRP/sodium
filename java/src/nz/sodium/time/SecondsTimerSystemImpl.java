package nz.sodium.time;

class SecondsTimerSystemImpl implements TimerSystemImpl<Double> {
    public SecondsTimerSystemImpl() {
        this(new MillisecondsTimerSystemImpl());
    }
    private SecondsTimerSystemImpl(TimerSystemImpl<Long> impl) {
        this.t0 = impl.now();
        this.impl = impl;
    }
    private final long t0;
    private final TimerSystemImpl<Long> impl;
    public Timer setTimer(Double t, Runnable callback) {
        return impl.setTimer((long)(t * 1000.0) + t0, callback);
    }
    public void runTimersTo(Double now) {
        impl.runTimersTo((long)(now * 1000.0) + t0);
    }
    public Double now() {
        return (double)(impl.now() - t0) / 1000.0;
    }
}

