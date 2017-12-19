using System;

namespace Sodium
{
    /// <summary>
    ///     A forward reference for a <see cref="DiscreteCell{T}" /> equivalent to the <see cref="DiscreteCell{T}" /> that is
    ///     referenced.
    /// </summary>
    /// <typeparam name="T">The type of values in the discrete cell loop.</typeparam>
    public class DiscreteCellLoop<T> : DiscreteCell<T>
    {
        private readonly StreamLoop<T> streamLoop;
        private readonly CellLoop<T> cellLoop;

        public DiscreteCellLoop()
            : this(new CellLoop<T>())
        {
        }

        private DiscreteCellLoop(CellLoop<T> cellLoop)
            : base(cellLoop)
        {
            this.streamLoop = new StreamLoop<T>();
            this.cellLoop = cellLoop;
        }

        /// <summary>
        ///     Resolve the loop to specify what the <see cref="DiscreteCellLoop{T}" /> was a forward reference to.  This method
        ///     must be called inside the same transaction as the one in which this <see cref="DiscreteCellLoop{T}" /> instance was
        ///     created and used.
        ///     This requires an explicit transaction to be created with <see cref="Transaction.Run{T}(Func{T})" /> or
        ///     <see cref="Transaction.RunVoid(Action)" />.
        /// </summary>
        /// <param name="c">The cell that was forward referenced.</param>
        public void Loop(DiscreteCell<T> c)
        {
            this.streamLoop.Loop(c.Updates);
            this.cellLoop.Loop(c.Cell);
        }

        public override Stream<T> Updates => this.streamLoop;

        /// <summary>
        ///     Return a reference to this <see cref="DiscreteCellLoop{T}" /> as a <see cref="DiscreteCell{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="DiscreteCellLoop{T}" /> as a <see cref="DiscreteCell{T}" />.</returns>
        public DiscreteCell<T> AsDiscreteCell() => this;
    }
}