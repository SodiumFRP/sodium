package sodium.time;

public interface TimerSystemImpl<T> {
    public Timer setTimer(T t, Runnable callback);
    public void runTimersTo(T t);
    public T now();
}

