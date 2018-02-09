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
        private Transaction transaction;

        private readonly object isLoopedLock = new object();
        private bool isLooped;

        public CellLoop()
        {
            this.transaction = Transaction.GetCurrentTransaction();

            if (this.transaction == null)
            {
                throw new InvalidOperationException("Loop must be created within an explicit transaction.");
            }

            this.transaction.Last(
                () =>
                {
                    if (this.transaction != null)
                    {
                        this.transaction = null;

                        throw new InvalidOperationException("Loop was not looped.");
                    }
                });
        }

        /// <summary>
        ///     Resolve the loop to specify what the <see cref="CellLoop{T}" /> was a forward reference to.  This method
        ///     must be called inside the same transaction as the one in which this <see cref="CellLoop{T}" /> instance was
        ///     created and used.
        ///     This requires an explicit transaction to be created with <see cref="Transaction.Run{T}(Func{T})" /> or
        ///     <see cref="Transaction.RunVoid(Action)" />.
        /// </summary>
        /// <param name="c">The cell that was forward referenced.</param>
        public void Loop(Cell<T> c) =>
            Transaction.Apply(
                trans =>
                {
                    lock (this.isLoopedLock)
                    {
                        if (this.isLooped)
                        {
                            throw new InvalidOperationException("Loop was looped more than once.");
                        }

                        this.isLooped = true;
                    }

                    if (trans != this.transaction)
                    {
                        this.transaction = null;

                        throw new InvalidOperationException("Loop must be looped in the same transaction that it was created in.");
                    }

                    this.transaction = null;

                    this.Loop(trans, c);

                    return Unit.Value;
                },
                false);
    }

    /// <summary>
    ///     A forward reference for a <see cref="Cell{T}" /> equivalent to the <see cref="Cell{T}" /> that is
    ///     referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell loop.</typeparam>
    public class LoopedCell<T> : Cell<T>
    {
        private readonly LoopedStream<T> streamLoop;
        private readonly LoopedBehavior<T> behaviorLoop;

        internal LoopedCell()
            : this(new LoopedBehavior<T>())
        {
        }

        private LoopedCell(LoopedBehavior<T> behaviorLoop)
            : base(behaviorLoop)
        {
            this.streamLoop = new LoopedStream<T>();
            this.behaviorLoop = behaviorLoop;
        }

        protected internal void Loop(Transaction trans, Cell<T> c)
        {
            this.streamLoop.Loop(trans, c.Updates);
            this.behaviorLoop.Loop(trans, c.Behavior);
        }

        public override Stream<T> Updates => this.streamLoop;

        /// <summary>
        ///     Return a reference to this <see cref="CellLoop{T}" /> as a <see cref="Cell{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="CellLoop{T}" /> as a <see cref="Cell{T}" />.</returns>
        public Cell<T> AsCell() => this;
    }
}