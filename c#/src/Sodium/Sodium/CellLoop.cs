using System;

namespace Sodium
{
    /// <summary>
    ///     A forward reference for a <see cref="Cell{T}" /> equivalent to the <see cref="Cell{T}" /> that is referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell.</typeparam>
    public class CellLoop<T> : CellLoopInternal<T>
    {
        /// <summary>
        ///     Create a <see cref="CellLoop{T}" />.
        /// </summary>
        public CellLoop()
            : base(new StreamLoop<T>())
        {
        }

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
            Transaction.Apply(trans =>
            {
                this.StreamLoop.Loop(c.Updates(trans));
                this.LazyInitialValue = c.SampleLazy(trans);
                return Unit.Value;
            });
        }

        internal override T SampleNoTransaction()
        {
            if (!this.StreamLoop.IsAssigned)
            {
                throw new InvalidOperationException("CellLoop was sampled before it was looped.");
            }

            return base.SampleNoTransaction();
        }
    }
}