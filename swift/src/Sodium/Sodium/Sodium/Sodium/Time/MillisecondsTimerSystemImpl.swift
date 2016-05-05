
extension dispatch_time_t {
    static func secondsFromNow(secs: Double) -> dispatch_time_t {
        return dispatch_time(dispatch_time_t(DISPATCH_TIME_NOW), Int64(secs * Double(NSEC_PER_SEC)))
    }
    static func nanosFromNow(nanos: Int64) -> dispatch_time_t {
        return dispatch_time(dispatch_time_t(DISPATCH_TIME_NOW), nanos)
    }
}



extension Int64: ITimerSystemImpl {

    class SimpleTimer: Timer {
        private var cancelled = false
        
        private init(nanos: Int64, callback: Block) {
            dispatch_after(dispatch_time_t.nanosFromNow(nanos), dispatch_get_main_queue(), {
                if !self.cancelled {
                    callback()
                }
            })
            
            self.seq = OSAtomicIncrement64(&SimpleTimer.nextSeq)
        }
        
        private var seq: Int64
        private static var nextSeq = Int64(0)
        
        func cancel() {
            self.cancelled = true
        }
    }

    // Mark: ITimerSystemImpl
     func setTimer(nanos: Int64, callback: Block) -> Timer {
        return SimpleTimer(nanos: nanos, callback: callback)
    }
    
    var now:Int64 {
        return Int64(dispatch_time_t(DISPATCH_TIME_NOW))
    }
}

