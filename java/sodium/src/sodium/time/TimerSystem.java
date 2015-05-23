package sodium.time;

import sodium.*;
import java.util.Optional;

public abstract class TimerSystem<T> {
    public abstract Timer setTimer(T t, Runnable callback);
    public abstract T now();

    public Cell<T> clock() {
        return new Cell<T>(null) {
            @Override
            protected T sampleNoTrans()
            {
                return now();
            }
        };
    }

    private static class CurrentTimer {
        Optional<Timer> oTimer = Optional.empty();
    };

    /**
     * A timer that fires at the specified time.
     */
    public Stream<Unit> at(Cell<Optional<T>> tAlarm) {
        StreamSink<Unit> sOut = new StreamSink<>();
        CurrentTimer current = new CurrentTimer();
        Listener l = tAlarm.listen(oAlarm -> {
            if (current.oTimer.isPresent())
                current.oTimer.get().cancel();
            current.oTimer = oAlarm.isPresent()
                ? Optional.<Timer>of(
                    setTimer(oAlarm.get(),
                             () -> { sOut.send(Unit.UNIT); }))
                : Optional.<Timer>empty();
        });
        return sOut.addCleanup(l);
    }
}

