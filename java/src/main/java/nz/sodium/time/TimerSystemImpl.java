package nz.sodium.time;

/**
 * An interface for implementations of FRP timer systems.
 */
public interface TimerSystemImpl<T> {
    /**
     * Set a timer that will execute the specified callback at the specified time.
     * @return A handle that can be used to cancel the timer.
     */
    public Timer setTimer(T t, Runnable callback);
    /**
     * Run all pending timers scheduled for up to and including the specified time.
     */
    public void runTimersTo(T t);
    /**
     * Return the current clock time.
     */
    public T now();
}

