package sodium.time;

import sodium.*;
import java.util.LinkedList;
import java.util.Optional;

public class TimerSystem<T extends Comparable> {
    public TimerSystem(TimerSystemImpl<T> impl) {
        this.impl = impl;
        CellSink<T> timeSnk = new CellSink<T>(impl.now());
        time = timeSnk;
        Transaction.onStart(() -> {
            T t = impl.now();
            impl.runTimersTo(t);
            while (true) {
                Event ev;
                // Pop all events earlier than t.
                synchronized (eventQueue) {
                    ev = eventQueue.peekFirst();
                    if (ev != null && ev.t.compareTo(t) <= 0)
                        eventQueue.removeFirst();
                    else
                        ev = null;
                }
                if (ev != null) {
                    timeSnk.send(ev.t);
                    ev.sAlarm.send(ev.t);
                }
                else
                    break;
            }
            timeSnk.send(t);
        });
    }

    private final TimerSystemImpl<T> impl;
    public final Cell<T> time;

    private class Event {
        Event(T t, StreamSink<T> sAlarm) {
            this.t = t;
            this.sAlarm = sAlarm;
        }
        T t;
        StreamSink<T> sAlarm;
    };
    private LinkedList<Event> eventQueue = new LinkedList<>();

    private static class CurrentTimer {
        Optional<Timer> oTimer = Optional.empty();
    };

    /**
     * A timer that fires at the specified time.
     */
    public Stream<T> at(Cell<Optional<T>> tAlarm) {
        StreamSink<T> sAlarm = new StreamSink<>();
        CurrentTimer current = new CurrentTimer();
        Listener l = tAlarm.listen(oAlarm -> {
            if (current.oTimer.isPresent())
                current.oTimer.get().cancel();
            current.oTimer = oAlarm.isPresent()
                ? Optional.<Timer>of(
                    impl.setTimer(oAlarm.get(), () -> {
                        synchronized (eventQueue) {
                            eventQueue.add(new Event(oAlarm.get(), sAlarm));
                        }
                        // Open and close a transaction to trigger queued
                        // events to run.
                        Transaction.runVoid(() -> {});
                    }))
                : Optional.<Timer>empty();
        });
        return sAlarm.addCleanup(l);
    }
}

