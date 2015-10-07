package nz.sodium.time;

/**
 * A timer system implementation using Java's {@link System#currentTimeMillis()} clock.
 */
public class MillisecondsTimerSystem extends TimerSystem<Long> {
    public MillisecondsTimerSystem() {
        super(new MillisecondsTimerSystemImpl());
    }
}

