/// <summary>
///     Helper methods for creating a <see cref="Cell{T}" />.
/// </summary>

public protocol CellType {
    associatedtype Element
    func stream() -> Stream<Element>
    func sample() -> Element
    func sampleLazy(trans: Transaction) -> Lazy<Element>
    func sampleNoTransaction() -> Element
    func value(trans: Transaction) -> Stream<Element>
}


public struct AnyCell<T>: CellType {
    private let _stream: () -> Stream<T>
    private let _sample: () -> T
    private let _sampleLazy: (Transaction)->Lazy<T>
    private let _sampleNoTransaction: () -> T
    private let _value: (Transaction) -> Stream<T>
    
    init<Base: CellType where T == Base.Element>(_ base: Base) {
        _stream = base.stream
        _sample = base.sample
        _sampleLazy = base.sampleLazy
        _sampleNoTransaction = base.sampleNoTransaction
        _value = base.value
    }
    
    public func stream() -> Stream<T> {
        return _stream()
    }
    public func sample() -> T {
        return _sample()
    }
    public func sampleLazy(trans: Transaction) -> Lazy<T> {
        return _sampleLazy(trans)
    }
    public func sampleNoTransaction() -> T {
        return _sampleNoTransaction()
    }
    public func value(trans: Transaction) -> Stream<T> {
        return _value(trans)
    }
}

//enum Value<T> {
//    case Initial(T)
//    case Updated(T)
//}

/// <summary>
///     Represents a value that changes over time.
/// </summary>
/// <typeparam name="T">The type of the value.</typeparam>
public class CellBase<T> : CellType {
    internal let _stream: Stream<T>
    private var _value: T
    private var _valueUpdate: T?
    
    /// <summary>
    ///     Creates a cell with a constant value.
    /// </summary>
    /// <typeparam name="T">The type of the value of the cell.</typeparam>
    /// <param name="value">The value of the cell.</param>
    /// <returns>A cell with a constant value.</returns>
    public static func constant<T>(value: T) -> Cell<T> {
        return Cell<T>(value: value)
    }
    
    /// <summary>
    ///     Creates a cell with a lazily computed constant value.
    /// </summary>
    /// <typeparam name="T">The type of the value of the cell.</typeparam>
    /// <param name="value">The lazily computed value of the cell.</param>
    /// <returns>A cell with a lazily computed constant value.</returns>
    public static func constantLazy<TResult>(@autoclosure(escaping) value: () -> TResult) -> AnyCell<TResult>
    {
        return Stream<TResult>.never().holdLazy(value)
    }

    /// <summary>
    ///     Creates a cell with a constant value.
    /// </summary>
    /// <param name="value">The constant value of the cell.</param>
    internal init(value: T)
    {
        self._stream = Stream<T>()
        self._value = value
    }
    
    internal init(stream: Stream<T>, initialValue: T) {
        self._stream = stream
        self._value = initialValue
    }

    internal var keepListenersAlive: IKeepListenersAlive { return self._stream.keepListenersAlive }

    var ValueProperty: T
    {
        get {
            return _value
        }
        set(value)
        {
            self._value = value
        }
    }

    /// <summary>
    ///     Sample the current value of the cell.
    /// </summary>
    /// <returns>The current value of the cell.</returns>
    /// <remarks>
    ///     <para>
    ///         This method may be used inside the functions passed to primitives that apply them to streams,
    ///         including <see cref="Stream{T}.Map{TResult}(Func{T, TResult})" /> in which case it is equivalent to
    ///         snapshotting the cell,
    ///         <see cref=" Stream{T}.Snapshot{T2, TResult}(Cell{T2}, Func{T, T2, TResult})" />,
    ///         <see cref="Stream{T}.Filter(Func{T, bool})" />, and
    ///         <see cref="Stream{T}.Merge(Stream{T}, Func{T, T, T})" />
    ///     </para>
    ///     <para>
    ///         It should generally be avoided in favor of <see cref="Listen(Action{T})" />
    ///         so updates aren't missed, but in many circumstances it makes sense.
    ///     </para>
    ///     <para>
    ///         It can be best to use this method inside an explicit transaction (using
    ///         <see cref="Transaction.Run{T}(Func{T})" /> or <see cref="Transaction.RunVoid(Action)" />).
    ///         For example, a b.Sample() inside an explicit transaction along with a b.Updates().Listen(...) will capture the
    ///         current value and any updates without risk of missing any in between.
    ///     </para>
    /// </remarks>
    public func sample() -> T {
        return  Transaction.apply{ trans in self.sampleNoTransaction() }
    }
    public func stream() -> Stream<T> {
        return self._stream
    }

    public func sampleLazy(trans: Transaction) -> Lazy<T> {
        let s = LazySample(cell: self)
        trans.last(
            {
                s.value = self._valueUpdate ?? self.sampleNoTransaction()
                //s.cell = nil
        })
        return Lazy(f: { s.value ?? s.cell.sample() })
    }

    /// <summary>
    ///     Sample the current value of the cell.
    /// </summary>
    /// <returns>A lazy which may be used to get the current value of the cell.</returns>
    /// <remarks>
    ///     This is a variant of <see cref="Sample" /> that works with the <see cref="CellLoop{T}" /> class
    ///     when the cell loop has not yet been looped.  It should be used in any code that is general
    ///     enough that it may be passed a <see cref="CellLoop{T}" />.  See <see cref="Stream{T}.HoldLazy(Lazy{T})" />.
    /// </remarks>
    public func sampleNoTransaction() -> T
    {
        let t = self.ValueProperty
        return t
    }

    internal func updates(trans: Transaction?) -> Stream<T> { return self.stream() }

    /// <summary>
    ///     Listen for updates to the value of this cell.  The returned <see cref="IListener" /> may be
    ///     disposed to stop listening, or it will automatically stop listening when it is garbage collected.
    ///     This is an OPERATIONAL mechanism for interfacing between the world of I/O and FRP.
    /// </summary>
    /// <param name="handler">The handler to execute for each value.</param>
    /// <returns>An <see cref="IListener" /> which may be disposed to stop listening.</returns>
    /// <remarks>
    ///     <para>
    ///         No assumptions should be made about what thread the handler is called on and it should not block.
    ///         Neither <see cref="StreamSink{T}.Send" /> nor <see cref="CellSink{T}.Send" /> may be called from the
    ///         handler.
    ///         They will throw an exception because this method is not meant to be used to create new primitives.
    ///     </para>
    ///     <para>
    ///         If the <see cref="IListener" /> is not disposed, it will continue to listen until this cell is either
    ///         disposed or garbage collected or the listener itself is garbage collected.
    ///     </para>
    /// </remarks>
    public func listenWeak(handler: (T) -> Void) -> Listener {
        return Transaction.apply { trans in self.value(trans).listenWeak(handler)}
    }




/*
    /// <summary>
    ///     Return a cell whose stream only receives events which have a different value than the previous event.
    /// </summary>
    /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
    public func calm() -> Cell<T>
    {
        return self.Calm(EqualityComparer<T>.Default)
    }

    /// <summary>
    ///     Return a cell whose stream only receives events which have a different value than the previous event.
    /// </summary>
    /// <param name="comparer">The equality comparer to use to determine if two items are equal.</param>
    /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
    public func calm(comparer: IEqualityComparer<T>) -> Cell<T>
    {
        let initA = self.SampleLazy()
        let mInitA = initA.Map(Maybe.Just)
        return Transaction.Apply { trans in self.Updates(trans).Calm(mInitA, comparer).HoldLazy(initA) }
    }
*/
}

private class LazySample<C:CellType>
{
    let cell: C
    var value: C.Element?
    
    init(cell: C)
    {
        self.cell = cell
    }
}

public class Cell<T>: CellBase<T> {
    private var cleanup: Listener = Listener(unlisten: nop)

    internal override init(value: T) {
        super.init (value: value)
        
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
    internal override init(stream: Stream<T>, initialValue: T) {

        super.init(stream: stream, initialValue: initialValue)
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
    
    deinit {
        self.cleanup.unlisten()
    }
}

extension CellType {
    //      /**
    //* Lift a binary function into cells, so the returned Cell always reflects the specified
    //* function applied to the input cells' values.
    //* @param f Function to apply. It must be <em>referentially transparent</em>.
    //*/
    
    /// <summary>
    ///     Lift a binary function into cells, so the returned cell always reflects the specified function applied to the input
    ///     cells' values.
    /// </summary>
    /// <typeparam name="T2">The type of second cell.</typeparam>
    /// <typeparam name="TResult">The type of the result.</typeparam>
    /// <param name="f">The binary function to lift into the cells.</param>
    /// <param name="b2">The second cell.</param>
    /// <returns>A cell containing values resulting from the binary function applied to the input cells' values.</returns>
    public func lift<C:CellType, TResult>(b2: C, f: (Element,C.Element) -> TResult) -> AnyCell<TResult> {
        let ffa = { a in { b in f(a,b) }}
        return b2.apply(self.map(ffa))
    }
    /// <summary>
    ///     Lift a ternary function into cells, so the returned cell always reflects the specified function applied to the
    ///     input cells' values.
    /// </summary>
    /// <typeparam name="T2">The type of second cell.</typeparam>
    /// <typeparam name="T3">The type of third cell.</typeparam>
    /// <typeparam name="TResult">The type of the result.</typeparam>
    /// <param name="f">The binary function to lift into the cells.</param>
    /// <param name="b2">The second cell.</param>
    /// <param name="b3">The third cell.</param>
    /// <returns>A cell containing values resulting from the ternary function applied to the input cells' values.</returns>
    public func lift<T2, T3, TResult>(b2: Cell<T2>, b3: Cell<T3>, f: (Element,T2,T3) -> TResult) -> AnyCell<TResult>
    {
        let ffa = { a in { b in { c in f(a,b,c) }}}
        return b3.apply(b2.apply(self.map(ffa)))
    }
    
    /// <summary>
    ///     Lift a quaternary function into cells, so the returned cell always reflects the specified function applied to the
    ///     input cells' values.
    /// </summary>
    /// <typeparam name="T2">The type of second cell.</typeparam>
    /// <typeparam name="T3">The type of third cell.</typeparam>
    /// <typeparam name="T4">The type of fourth cell.</typeparam>
    /// <typeparam name="TResult">The type of the result.</typeparam>
    /// <param name="f">The binary function to lift into the cells.</param>
    /// <param name="b2">The second cell.</param>
    /// <param name="b3">The third cell.</param>
    /// <param name="b4">The fourth cell.</param>
    /// <returns>A cell containing values resulting from the quaternary function applied to the input cells' values.</returns>
    public func lift<T2, T3, T4, TResult>(b2: Cell<T2>, b3: Cell<T3>, b4: Cell<T4>, f: (Element,T2,T3,T4) -> TResult) -> AnyCell<TResult>
    {
        let ffa = { a in { b in { c in { d in f(a,b,c,d) }}}}
        return b4.apply(b3.apply(b2.apply(self.map(ffa))))
    }
    
    /// <summary>
    ///     Lift a 5-argument function into cells, so the returned cell always reflects the specified function applied to the
    ///     input cells' values.
    /// </summary>
    /// <typeparam name="T2">The type of second cell.</typeparam>
    /// <typeparam name="T3">The type of third cell.</typeparam>
    /// <typeparam name="T4">The type of fourth cell.</typeparam>
    /// <typeparam name="T5">The type of fifth cell.</typeparam>
    /// <typeparam name="TResult">The type of the result.</typeparam>
    /// <param name="f">The binary function to lift into the cells.</param>
    /// <param name="b2">The second cell.</param>
    /// <param name="b3">The third cell.</param>
    /// <param name="b4">The fourth cell.</param>
    /// <param name="b5">The fifth cell.</param>
    /// <returns>A cell containing values resulting from the 5-argument function applied to the input cells' values.</returns>
    public func lift<T2, T3, T4, T5, TResult>(b2: Cell<T2>, b3: Cell<T3>, b4: Cell<T4>, b5: Cell<T5>, f: (Element,T2,T3,T4,T5)->TResult) -> AnyCell<TResult>
    {
        let ffa = { a in { b in { c in { d in { e in f(a,b,c,d,e) }}}}}
        return b5.apply(b4.apply(b3.apply(b2.apply(self.map(ffa)))))
    }
    
    /// <summary>
    ///     Lift a 6-argument function into cells, so the returned cell always reflects the specified function applied to the
    ///     input cells' values.
    /// </summary>
    /// <typeparam name="T2">The type of second cell.</typeparam>
    /// <typeparam name="T3">The type of third cell.</typeparam>
    /// <typeparam name="T4">The type of fourth cell.</typeparam>
    /// <typeparam name="T5">The type of fifth cell.</typeparam>
    /// <typeparam name="T6">The type of sixth cell.</typeparam>
    /// <typeparam name="TResult">The type of the result.</typeparam>
    /// <param name="f">The binary function to lift into the cells.</param>
    /// <param name="b2">The second cell.</param>
    /// <param name="b3">The third cell.</param>
    /// <param name="b4">The fourth cell.</param>
    /// <param name="b5">The fifth cell.</param>
    /// <param name="b6">The sixth cell.</param>
    /// <returns>A cell containing values resulting from the 6-argument function applied to the input cells' values.</returns>
    public func lift<T2, T3, T4, T5, T6, TResult>(b2: Cell<T2>, b3: Cell<T3>, b4: Cell<T4>, b5: Cell<T5>, b6: Cell<T6>, f: (Element,T2,T3,T4,T5,T6)->TResult) -> AnyCell<TResult>
    {
        let ffa = { a in { b in { c in { d in { e in { _f in f(a,b,c,d,e,_f) }}}}}}
        return b6.apply(b5.apply(b4.apply(b3.apply(b2.apply(self.map(ffa))))))
    }
    
    /// <summary>
    ///     Apply a value inside a cell to a function inside a cell.  This is the primitive for all function lifting.
    /// </summary>
    /// <typeparam name="TResult">The type of the result.</typeparam>
    /// <param name="bf">The cell containing the function to apply the value to.</param>
    /// <returns>
    ///     A cell whose value is the result of applying the current function in cell <paramref name="bf" /> to this
    ///     cell's current value.
    /// </returns>
    //public func apply<TResult, C:CellType where C.Element == Element->TResult>(bf: C) -> AnyCell<TResult> {
   // }
    
    public func apply<TResult, C:CellType where C.Element == Element->TResult>(bf: C) -> AnyCell<TResult> {
        return Transaction.apply{ trans0 in
            let out = Stream<TResult>(keepListenersAlive: self.stream().keepListenersAlive)
            
            let outTarget = out.node
            let inTarget = Node<TResult>(rank: 0)
            let nodeTarget = inTarget.link({ (t, v, dbg) in }, target: outTarget).1
            
            var f: ((Element)->TResult)?
            var a: Element?
            
            let h = { (trans1: Transaction) -> Void in
                trans1.prioritized(out.node as INode, action: { trans2 throws -> Void in out.send(trans2, a: f!(a!))}, dbg: "Cell<>.apply()" )}
            
            let l1 = bf.value(trans0).listen(inTarget, action: {(trans1, ff, dbg) in
                f = ff
                if a != nil {
                    h(trans1)
                }
            })
            let l2 = self.value(trans0).listen(inTarget, action: { (trans1, aa, dbg) in
                a = aa
                if f != nil {
                    h(trans1)
                }
            })
            return out.lastFiringOnly(trans0).unsafeAddCleanup([l1,l2,
                Listener(unlisten: { inTarget.unlink(nodeTarget) })]).holdLazy({ bf.sampleNoTransaction()(self.sampleNoTransaction()) })
        }
    }

    /*
     * Listen for updates to the value of this cell.  The returned Listener may be disposed to stop listening.  
     * This is an OPERATIONAL mechanism for interfacing between the world of I/O and FRP.
     * 
     * handler - The handler to execute for each value.
     * returns - Listener which may be disposed to stop listening.
     *
     * No assumptions should be made about what thread the handler is called on and it should not block.  Neither
     * StreamSink<T>.send nor CellSink<T>.send may be called from the handler.  They will throw an exception
     * because this method is not meant to be used to create new primitives.
     *
     * If the Listener is not disposed, it will continue to listen until this cell is disposed.
     */
  
    public func listen(handler: (Element) -> Void) -> Listener {
        return Transaction.apply{trans in self.value(trans).listen(handler)}!
    }

    /*
     * Transform the cell values according to the supplied function, so the returned cell's values reflect the
     * value of the function applied to the input cell's values.
     *
     * TResult - The type of values fired by the returned cell.
     * f - Function to apply to convert the values.  It must be a pure function.
     *
     * returns - An cell which fires values transformed by f() for each value fired by this cell.
     */
    public func map<TResult>(f: (Element) -> TResult) -> Cell<TResult>
    {
    //    let foo = Transaction.apply{ (trans: Transaction) in
    //        self.stream().map(f).holdLazy(trans, lazy: self.sampleLazy(trans).map(f)) }

        let foo = Transaction.apply{ (trans: Transaction) in
            self.stream().map(f).hold(f(self.sample())) }

        return foo
    }
    
    public func value(trans1: Transaction) -> Stream<Element> {
        let spark = Stream<Unit>(keepListenersAlive: self.stream().keepListenersAlive)
        trans1.prioritized(spark.node, action: { trans2 in spark.send(trans2, a: Unit.value)}, dbg: "Cell.value()")
        let initial = spark.snapshot(self)
        //return initial.merge(self.updates(trans1), f: { $1 })
        return initial.merge(self.stream(), f: { $1 })
    }
}