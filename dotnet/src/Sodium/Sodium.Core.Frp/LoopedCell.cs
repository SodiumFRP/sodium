namespace Sodium.Frp
{
    /// <summary>
    ///     A forward reference for a <see cref="Cell{T}" /> equivalent to the <see cref="Cell{T}" /> that is
    ///     referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell loop.</typeparam>
    public class LoopedCell<T> : Cell<T>
    {
        private readonly LoopedBehavior<T> behaviorLoop;

        internal LoopedCell()
            : this(new LoopedBehavior<T>())
        {
        }

        private LoopedCell(LoopedBehavior<T> behaviorLoop)
            : base(behaviorLoop) =>
            this.behaviorLoop = behaviorLoop;

        internal void Loop(TransactionInternal trans, Cell<T> c) => this.behaviorLoop.Loop(trans, c.BehaviorImpl);
    }
}