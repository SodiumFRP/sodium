package sodium.time;

public class SecondsTimerSystem extends TimerSystem<Double> {
    public SecondsTimerSystem() {
        super(new SecondsTimerSystemImpl());
    }
}

