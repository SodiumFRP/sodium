
public class LazyCell<T> : CellType
{
    internal var cleanup: Listener = Listener(unlisten: nop)
    internal let _stream: Stream<T>
    internal var LazyInitialValue: Lazy<T>
    internal lazy var _value: T = self.LazyInitialValue**
    internal var _valueUpdate: T?
    
    init(stream: Stream<T>, @autoclosure(escaping) autoInitialValue: () -> T)
    {
        self.LazyInitialValue = Lazy<T>(f: autoInitialValue)
        self._stream = stream
        self.cleanup = Transaction.apply{ trans1 in
            self._stream.listen(Node<T>.Null, trans: trans1, action: { (trans2, a, dbg) in
                if self._valueUpdate == nil {
                    trans2.last({
                        self._value = self._valueUpdate!
                        self._valueUpdate = nil
                    })
                }
                self._valueUpdate = a
                }, suppressEarlierFirings: false)
        }
    }
    
    init(stream: Stream<T>, lazyInitialValue: () -> T)
    {
        self.LazyInitialValue = Lazy<T>(f: lazyInitialValue)
        self._stream = stream
        self.cleanup = Transaction.apply{ trans1 in
            self._stream.listen(Node<T>.Null, trans: trans1, action: { (trans2, a, dbg) in
                if self._valueUpdate == nil {
                    trans2.last({
                        self._value = self._valueUpdate!
                        self._valueUpdate = nil
                    })
                }
                self._valueUpdate = a
                }, suppressEarlierFirings: false)
        }
    }

    public init(stream: Stream<T>, lazyInitialValue: Lazy<T>)
    {
        self.LazyInitialValue = lazyInitialValue
        self._stream = stream
        self.cleanup = Transaction.apply{ trans1 in
            self._stream.listen(Node<T>.Null, trans: trans1, action: { (trans2, a, dbg) in
                if self._valueUpdate == nil {
                    trans2.last({
                        self._value = self._valueUpdate!
                        self._valueUpdate = nil
                    })
                }
                self._valueUpdate = a
                }, suppressEarlierFirings: false)
        }
    }
    
    public func stream() -> Stream<T> {
        return Stream<T>()
    }
    public func sample() -> T {
        return self._value
    }
    public func sampleLazy(trans: Transaction) -> Lazy<T> {
        return self.LazyInitialValue
    }
   
    public func value(trans: Transaction) -> Stream<T> {
        let spark = Stream<Unit>(keepListenersAlive: self._stream.keepListenersAlive)
        trans.prioritized(spark.node, action: { trans2 in spark.send(trans2, a: Unit.value)})
        let initial = spark.snapshot(self)
        return initial.merge(self._stream, f: { $1 })
    }

    public func sampleNoTransaction() -> T {
        return self._value
    }
}

postfix operator ** { }
postfix func **<T>(l: Lazy<T>) -> T { return l.get() }
