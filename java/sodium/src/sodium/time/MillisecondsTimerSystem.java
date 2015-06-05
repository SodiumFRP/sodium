package sodium.time;

public class MillisecondsTimerSystem extends TimerSystem<Long> {
    public MillisecondsTimerSystem() {
        super(new MillisecondsTimerSystemImpl());
    }
}

