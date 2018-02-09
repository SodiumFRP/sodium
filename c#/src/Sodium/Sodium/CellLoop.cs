using System;

namespace Sodium
{
    /// <summary>
    ///     A forward reference for a <see cref="Cell{T}" /> equivalent to the <see cref="Cell{T}" /> that is
    ///     referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell loop.</typeparam>
    public class CellLoop<T> : LoopedCell<T>
    {
        /// <summary>
        ///     Resolve the loop to specify what the <see cref="CellLoop{T}" /> was a forward reference to.  This method
        ///     must be called inside the same transaction as the one in which this <see cref="CellLoop{T}" /> instance was
        ///     created and used.
        ///     This requires an explicit transaction to be created with <see cref="Transaction.Run{T}(Func{T})" /> or
        ///     <see cref="Transaction.RunVoid(Action)" />.
        /// </summary>
        /// <param name="c">The cell that was forward referenced.</param>
        public new void Loop(Cell<T> c) => base.Loop(c);
    }

    /// <summary>
    ///     A forward reference for a <see cref="Cell{T}" /> equivalent to the <see cref="Cell{T}" /> that is
    ///     referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell loop.</typeparam>
    public class LoopedCell<T> : Cell<T>
    {
        private readonly StreamLoop<T> streamLoop;
        private readonly BehaviorLoop<T> behaviorLoop;

        internal LoopedCell()
            : this(new BehaviorLoop<T>())
        {
        }

        private LoopedCell(BehaviorLoop<T> behaviorLoop)
            : base(behaviorLoop)
        {
            this.streamLoop = new StreamLoop<T>();
            this.behaviorLoop = behaviorLoop;
        }

        protected internal void Loop(Cell<T> c)
        {
            this.streamLoop.Loop(c.Updates);
            this.behaviorLoop.Loop(c.Behavior);
        }

        public override Stream<T> Updates => this.streamLoop;

        /// <summary>
        ///     Return a reference to this <see cref="CellLoop{T}" /> as a <see cref="Cell{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="CellLoop{T}" /> as a <see cref="Cell{T}" />.</returns>
        public Cell<T> AsCell() => this;
    }
}