package nz.sodium.time;

/**
 * A timer system implementation where the clock is a floating point number of seconds
 * since program start.
 */
public class SecondsTimerSystem extends TimerSystem<Double> {
    public SecondsTimerSystem() {
        super(new SecondsTimerSystemImpl());
    }
}

