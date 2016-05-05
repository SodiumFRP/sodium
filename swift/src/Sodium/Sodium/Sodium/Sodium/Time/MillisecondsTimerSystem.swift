/**
 * A timer system implementation using Java's {@link System#currentTimeMillis()} clock.
 */
class MillisecondsTimerSystem: TimerSystem<Int64> {
    public init() {
        super(MillisecondsTimerSystemImpl())
    }
}

