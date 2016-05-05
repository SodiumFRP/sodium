/// <summary>
///     A forward reference for a <see cref="Cell{T}" /> equivalent to the <see cref="Cell{T}" /> that is referenced.
/// </summary>
/// <typeparam name="T">The type of values in the cell.</typeparam>
public class CellLoop<T> : LazyCell<T>
{
    private let streamLoop: StreamLoop<T>

    /// <summary>
    ///     Create a <see cref="CellLoop{T}" />.
    /// </summary>
    public convenience init()
    {
        self.init(streamLoop: StreamLoop<T>())
    }

    private init(streamLoop: StreamLoop<T>)
    {
        self.streamLoop = streamLoop
        super.init(stream: streamLoop)
    }

    /// <summary>
    ///     Resolve the loop to specify what the <see cref="CellLoop{T}" /> was a forward reference to.  This method
    ///     must be called inside the same transaction as the one in which this <see cref="CellLoop{T}" /> instance was
    ///     created and used.
    ///     This requires an explicit transaction to be created with <see cref="Transaction.Run{T}(Func{T})" /> or
    ///     <see cref="Transaction.RunVoid(Action)" />.
    /// </summary>
    /// <param name="c">The cell that was forward referenced.</param>
    public func Loop(c: Cell<T>) {
        Transaction.Apply(
        { trans -> Unit in
            self.streamLoop.Loop(c.stream())
            self.LazyInitialValue = c.sampleLazy(trans)
            return Unit.value
        })
    }

    override public func sampleNoTransaction() -> T {
        if (!self.streamLoop.IsAssigned) {
            fatalError("CellLoop was sampled before it was looped.")
        }

        return super.sampleNoTransaction()
    }
}


func foo() {
    let sDelta = Stream<Int>()
    
    let sUpdate = sDelta.Snapshot(, f: <#T##(Int, T1) -> TResult#>), a: <#T##Int#>
}