using System;

namespace Sodium
{
    /// <summary>
    ///     A stream that allows values to be pushed into it which is meant to be used as the input update stream for a
    ///     <see cref="CellSink{T}" />.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell sink.</typeparam>
    public class CellStreamSink<T> : StreamSink<T>
    {
        /// <summary>
        ///     Construct a stream sink for use with a cell that uses the last value if <see cref="StreamSink{T}.Send" /> is called more
        ///     than once per transaction.
        /// </summary>
        public CellStreamSink()
            : base((left, right) => right)
        {
        }

        /// <summary>
        ///     Construct a stream sink for use with a cell that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="StreamSink{T}.Send" /> is called more than once per transaction.
        /// </summary>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="StreamSink{T}.Send" /> is called more than once per
        ///     transaction.
        /// </param>
        public CellStreamSink(Func<T, T, T> coalesce)
            : base(coalesce)
        {
        }

        /// <summary>
        ///     Return a reference to this <see cref="CellStreamSink{T}" /> as a <see cref="StreamSink{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="CellStreamSink{T}" /> as a <see cref="StreamSink{T}" />.</returns>
        public StreamSink<T> AsStreamSink() => this;
    }
}