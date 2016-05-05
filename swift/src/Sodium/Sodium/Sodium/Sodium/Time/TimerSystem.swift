public class TimerSystem<T: Comparable> {

    public init(impl: TimerSystemImpl) {
        self.impl = impl
        let timeSnk = CellSink<T>(impl.now)
        time = timeSnk
        
        Transaction.onStart(Runnable() {
            public void run() {
                T t = impl.now
                impl.runTimersTo(t)
                while (true) {
                    Event ev
                    // Pop all events earlier than t.
                    synchronized (eventQueue) {
                        ev = eventQueue.peekFirst()
                        if (ev != nil && ev.t.compareTo(t) <= 0)
                            eventQueue.removeFirst()
                        else
                            ev = nil
                    }
                    if (ev != nil) {
                        timeSnk.send(ev.t)
                        ev.sAlarm.send(ev.t)
                    }
                    else {
                        break
                    }
                }
                timeSnk.send(t)
            }
        })
    }

    private let impl: TimerSystemImpl
    
    /**
     * A cell giving the current clock time.
     */
    public let time: Cell<T>

    private class Event {
        init(t: T, sAlarm: StreamSink<T>) {
            this.t = t
            this.sAlarm = sAlarm
        }
        let t: T
        let sAlarm: StreamSink<T>
    }
    
    private let eventQueue = [Event]()

    private static class CurrentTimer {
        Optional<Timer> oTimer = Optional.empty()
    }

    /**
     * A timer that fires at the specified time.
     */
    public func at(tAlarm: Cell<T?>) -> Stream<T> {
        let sAlarm = StreamSink<T>()
        let current = CurrentTimer()
        let l = tAlarm.listen(Handler<Optional<T>>() {
            public void run(final Optional<T> oAlarm) {
                if (current.oTimer.isPresent())
                    current.oTimer.get().cancel()
                current.oTimer = oAlarm.isPresent()
                    ? Optional.<Timer>of(
                        impl.setTimer(oAlarm.get(), Runnable() {
                            public void run() {
                                synchronized (eventQueue) {
                                    eventQueue.add(Event(oAlarm.get(), sAlarm))
                                }
                                // Open and close a transaction to trigger queued
                                // events to run.
                                Transaction.runVoid(Runnable() {
                                    public void run() { }
                                })
                            }
                        }))
                    : Optional.<Timer>empty()
            }
        })
        return sAlarm.addCleanup(l)
    }
}

