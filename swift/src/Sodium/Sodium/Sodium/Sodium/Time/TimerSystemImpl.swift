
/**
 * An interface for implementations of FRP timer systems.
 */
protocol ITimerSystemImpl {
    /**
     * Set a timer that will execute the specified callback at the specified time.
     * @return A handle that can be used to cancel the timer.
     */
    func setTimer(t: Self, callback: Block) -> Timer
    /**
     * Return the current clock time.
     */
    var now: Self { get }
}

