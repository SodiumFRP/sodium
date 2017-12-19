using System;

namespace Sodium
{
    /// <summary>
    ///     A forward reference for a <see cref="Cell{T}" /> equivalent to the <see cref="Cell{T}" /> that is referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell loop.</typeparam>
    public class CellLoop<T> : LazyCell<T>
    {
        private readonly StreamLoop<T> streamLoop;

        public CellLoop()
            : this(new StreamLoop<T>())
        {
        }

        private CellLoop(StreamLoop<T> streamLoop)
            : base(streamLoop, null) => this.streamLoop = streamLoop;

        /// <summary>
        ///     Resolve the loop to specify what the <see cref="CellLoop{T}" /> was a forward reference to.  This method
        ///     must be called inside the same transaction as the one in which this <see cref="CellLoop{T}" /> instance was
        ///     created and used.
        ///     This requires an explicit transaction to be created with <see cref="Transaction.Run{T}(Func{T})" /> or
        ///     <see cref="Transaction.RunVoid(Action)" />.
        /// </summary>
        /// <param name="c">The cell that was forward referenced.</param>
        public void Loop(Cell<T> c)
        {
            Transaction.Apply(
                trans =>
                {
                    this.streamLoop.Loop(c.Updates(trans));
                    this.LazyInitialValue = c.SampleLazy(trans);
                    return Unit.Value;
                },
                false);
        }

        /// <summary>
        ///     Return a reference to this <see cref="CellLoop{T}" /> as a <see cref="Cell{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="CellLoop{T}" /> as a <see cref="Cell{T}" />.</returns>
        public Cell<T> AsCell() => this;

        internal override T SampleNoTransaction()
        {
            if (!this.streamLoop.IsAssigned)
            {
                throw new InvalidOperationException("CellLoop was sampled before it was looped.");
            }

            return base.SampleNoTransaction();
        }
    }
}