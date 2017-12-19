using System;

namespace Sodium
{
    /// <summary>
    ///     A discrete cell that allows values to be pushed into it, acting as an interface between the world of I/O and the
    ///     world of
    ///     FRP.  Code that exports instances of <see cref="DiscreteCellSink{T}" /> for read-only use should downcast to
    ///     <see cref="DiscreteCell{T}" />.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell sink.</typeparam>
    public class DiscreteCellSink<T> : DiscreteCell<T>
    {
        public DiscreteCellSink(T initialValue)
            : this(new DiscreteCellStreamSink<T>(), initialValue)
        {
        }

        private DiscreteCellSink(DiscreteCellStreamSink<T> streamSink, T initialValue)
            : this(streamSink, new Cell<T>(streamSink, initialValue))
        {
        }

        private DiscreteCellSink(DiscreteCellStreamSink<T> streamSink, Cell<T> cell)
            : base(cell) => this.StreamSink = streamSink;

        /// <summary>
        ///     The underlying stream sink providing discrete updates to this cell and through which new values are sent.
        /// </summary>
        public DiscreteCellStreamSink<T> StreamSink { get; }

        /// <summary>
        ///     Send a value, modifying the value of the cell.  This method may not be called from inside handlers registered with
        ///     <see cref="Stream{T}.Listen(Action{T})" /> or <see cref="DiscreteCell{T}.Listen(Action{T})" />.
        ///     An exception will be thrown, because sinks are for interfacing I/O to FRP only.  They are not meant to be used to
        ///     define new primitives.
        /// </summary>
        /// <param name="a">The value to send.</param>
        public void Send(T a) => this.StreamSink.Send(a);

        /// <summary>
        ///     Return a reference to this <see cref="DiscreteCellSink{T}" /> as a <see cref="DiscreteCell{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="DiscreteCellSink{T}" /> as a <see cref="DiscreteCell{T}" />.</returns>
        public DiscreteCell<T> AsDiscreteCell() => this;
    }
}