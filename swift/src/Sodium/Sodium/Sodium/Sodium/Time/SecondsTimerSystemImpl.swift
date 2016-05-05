class SecondsTimerImpl: TimerSystemImpl<Double> {
}

class TimerSystemImpl<T>: ITimerSystemImpl {
    
    convenience init() {
        self.init(impl: MillisecondsTimerSystemImpl())
    }
    
    private init(impl: ITimerSystemImpl) {
        self.t0 = impl.now
        self.impl = impl
    }
    private let t0 = T
    private let impl: ITimerSystemImpl
    
    // Mark: ITimerSystemImpl
    func setTimer(t: T, callback: Action) -> Timer {
        return impl.setTimer((T)(t * 1000.0) + t0, callback)
    }
    
    public func runTimersTo(now: T) {
        impl.runTimersTo((T)(now * 1000.0) + t0)
    }
    
    public var now: T {
        return (T)(impl.now - t0) / 1000.0
    }
}

