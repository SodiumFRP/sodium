/// <summary>
///     Helper methods for creating a <see cref="Cell{T}" />.
/// </summary>

public protocol CellType {
    typealias Element
    func stream() -> Stream<Element>
    func sample() -> Element
    func sampleLazy(trans: Transaction) -> Lazy<Element>
    func sampleNoTransaction() -> Element
    func value(trans: Transaction?) -> Stream<Element>
}


public struct AnyCell<T>: CellType {
    private let _stream: () -> Stream<T>
    private let _sample: () -> T
    private let _sampleLazy: (Transaction)->Lazy<T>
    private let _sampleNoTransaction: () -> T
    private let _value: (Transaction?) -> Stream<T>
    
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
    public func value(trans: Transaction?) -> Stream<T> {
        return _value(trans)
    }
}

enum Value<T> {
    case Initial(T)
    case Updated(T)
}

/// <summary>
///     Represents a value that changes over time.
/// </summary>
/// <typeparam name="T">The type of the value.</typeparam>
public class CellBase<T> : CellType {
    internal let _stream: Stream<T>
    private var value: Value<T>

    /// <summary>
    ///     Creates a cell with a constant value.
    /// </summary>
    /// <typeparam name="T">The type of the value of the cell.</typeparam>
    /// <param name="value">The value of the cell.</param>
    /// <returns>A cell with a constant value.</returns>
    public static func Constant<T>(value: T) -> Cell<T> {
        return Cell<T>(value: value)
    }
    
    /// <summary>
    ///     Creates a cell with a lazily computed constant value.
    /// </summary>
    /// <typeparam name="T">The type of the value of the cell.</typeparam>
    /// <param name="value">The lazily computed value of the cell.</param>
    /// <returns>A cell with a lazily computed constant value.</returns>
    public static func ConstantLazy<TResult>(@autoclosure(escaping) value: () -> TResult) -> AnyCell<TResult>
    {
        return Stream<TResult>.Never().HoldLazy(value)
    }

    /// <summary>
    ///     Creates a cell with a constant value.
    /// </summary>
    /// <param name="value">The constant value of the cell.</param>
    internal init(value: T)
    {
        self._stream = Stream<T>()
        self.value = .Initial(value)
    }
    
    internal init(stream: Stream<T>, initialValue: T) {
        
        self.value = .Initial(initialValue)
        self._stream = stream
    }

    internal var KeepListenersAlive: IKeepListenersAlive { return self._stream.KeepListenersAlive }

    var ValueProperty: T
    {
        get {
            switch self.value {
            case .Initial(let t):
                return t
            case .Updated(let t):
                return t
            }
        }
        set(value)
        {
            self.value = .Updated(value)
        }
    }

    deinit
    {
        //using (self.cleanup)
       // {
        //}
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
    //public func Sample() -> V { return  Transaction.Apply{ trans in self.SampleNoTransaction()} }
    public func sample() -> T {
        return  Transaction.Apply{ trans in self.sampleNoTransaction() }
    }
    public func stream() -> Stream<T> {
        return self._stream
    }

    public func sampleLazy(trans: Transaction) -> Lazy<T> {
        let s = LazySample(cell: self)
        trans.Last(
            {
                if case .Updated(let t) = self.value {
                    s.Value = t
                }
                else {
                    s.Value = self.sampleNoTransaction()
                }
                //s.cell = nil
        })
        return Lazy(f: { s.Value ?? s.cell.sample() })
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
    //public func SampleLazy() -> () -> T { return { Transaction.Apply(self.SampleLazy) } }
    
    public func sampleNoTransaction() -> T
    {
        return self.ValueProperty
    }

    internal func updates(trans: Transaction?) -> Stream<T> { return self.stream() }

    public func value(trans1: Transaction?) -> Stream<T> {
        let spark = Stream<Unit>(keepListenersAlive: self._stream.KeepListenersAlive)
        trans1!.Prioritized(spark.node, action: { trans2 in spark.Send(trans2, a: Unit.value)})
        let initial = spark.Snapshot(self)
        return initial.Merge(self.updates(trans1), f: { (left, right) in right })
    }

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
    public func ListenWeak(handler: (T) -> Void) -> Listener { return Transaction.Apply{ trans in self.value(trans).ListenWeak(handler)} }

    /// <summary>
    ///     Listen for updates to the value of this cell.  The returned <see cref="IListener" /> may be
    ///     disposed to stop listening.  This is an OPERATIONAL mechanism for interfacing between
    ///     the world of I/O and FRP.
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
    ///         disposed or garbage collected.
    ///     </para>
    /// </remarks>
    public func Listen(handler: (T) -> Void) -> Listener { return Transaction.Apply{trans in self.value(trans).Listen(handler)}! }



/*
    /// <summary>
    ///     Return a cell whose stream only receives events which have a different value than the previous event.
    /// </summary>
    /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
    public func Calm() -> Cell<T>
    {
        return self.Calm(EqualityComparer<T>.Default)
    }

    /// <summary>
    ///     Return a cell whose stream only receives events which have a different value than the previous event.
    /// </summary>
    /// <param name="comparer">The equality comparer to use to determine if two items are equal.</param>
    /// <returns>A cell whose stream only receives events which have a different value than the previous event.</returns>
    public func Calm(comparer: IEqualityComparer<T>) -> Cell<T>
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
    var Value: C.Element?
    
    init(cell: C)
    {
        self.cell = cell
    }
}

public class Cell<T>: CellBase<T> {
    private var cleanup: Listener = Listener(unlisten: nop)

    internal override init(value: T) {
        super.init (value: value)
        
        self.cleanup = Transaction.Apply{ trans1 in
            self._stream.Listen(Node<T>.Null, trans: trans1, action: { (trans2, a) in
                self.value = .Updated(a)
                }, suppressEarlierFirings: false)
        }

    }
    internal override init(stream: Stream<T>, initialValue: T) {

        super.init(stream: stream, initialValue: initialValue)
        self.cleanup = Transaction.Apply{ trans1 in
            self._stream.Listen(Node<T>.Null, trans: trans1, action: { (trans2, a) in
                self.value = .Updated(a)
                }, suppressEarlierFirings: false)
        }
    }
    
    deinit {
        self.cleanup.Unlisten()
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
    public func lift<T2, TResult>(b2: Cell<T2>, f: (Element,T2) -> TResult) -> AnyCell<TResult> {
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
    public func Lift<T2, T3, T4, T5, TResult>(b2: Cell<T2>, b3: Cell<T3>, b4: Cell<T4>, b5: Cell<T5>, f: (Element,T2,T3,T4,T5)->TResult) -> AnyCell<TResult>
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
    public func Lift<T2, T3, T4, T5, T6, TResult>(b2: Cell<T2>, b3: Cell<T3>, b4: Cell<T4>, b5: Cell<T5>, b6: Cell<T6>, f: (Element,T2,T3,T4,T5,T6)->TResult) -> AnyCell<TResult>
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
    public func apply<TResult>(bf: AnyCell<Element->TResult>) -> AnyCell<TResult> {
        return Transaction.Apply{ trans0 in
            let out = Stream<TResult>(keepListenersAlive: self.stream().KeepListenersAlive)
            
            let outTarget = out.node
            let inTarget = Node<TResult>(rank: 0)
            let nodeTarget = inTarget.Link({ (t, v) in }, target: outTarget).1
            
            var f: ((Element)->TResult)?
            var a: Element?
            
            let h = { (trans1: Transaction) -> Void in trans1.Prioritized(out.node as INode, action: { trans2 throws -> Void in out.Send(trans2, a: f!(a!))} )}
            
            let l1 = bf.value(trans0).Listen(inTarget, action: {(trans1, ff) in
                f = ff
                if a != nil {
                    h(trans1)
                }
            })
            let l2 = self.value(trans0).Listen(inTarget, action: { (trans1, aa) in
                a = aa
                if f != nil {
                    h(trans1)
                }
            })
            return out.LastFiringOnly(trans0).UnsafeAddCleanup([l1,l2,
                Listener(unlisten: { inTarget.Unlink(nodeTarget) })]).HoldLazy({ bf.sampleNoTransaction()(self.sampleNoTransaction()) })
        }
    }

    /// <summary>
    ///     Transform the cell values according to the supplied function, so the returned
    ///     cell's values reflect the value of the function applied to the input cell's values.
    /// </summary>
    /// <typeparam name="TResult">The type of values fired by the returned cell.</typeparam>
    /// <param name="f">
    ///     Function to apply to convert the values.  It must be a pure function.
    /// </param>
    /// <returns>An cell which fires values transformed by <paramref name="f" /> for each value fired by this cell.</returns>
    public func map<TResult>(f: (Element) -> TResult) -> AnyCell<TResult>
    {
        let foo = Transaction.Apply{ (trans: Transaction) in
            self.stream().Map(f).HoldLazy(trans, lazy: self.sampleLazy(trans).map(f)) }
        
        return foo
    }
    


}