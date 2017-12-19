using System;

namespace Sodium
{
    /// <summary>
    ///     A stream that allows values to be pushed into it which is meant to be used as the input update stream for a
    ///     <see cref="DiscreteCellSink{T}" />.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell sink.</typeparam>
    public class DiscreteCellStreamSink<T> : StreamSink<T>
    {
        /// <summary>
        ///     Construct a DiscreteCellStreamSink that uses the last value if <see cref="StreamSink{T}.Send" /> is called more
        ///     than once per transaction.
        /// </summary>
        public DiscreteCellStreamSink()
            : base((left, right) => right)
        {
        }

        /// <summary>
        ///     Construct a DiscreteCellStreamSink that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="StreamSink{T}.Send" /> is called more than once per transaction.
        /// </summary>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="StreamSink{T}.Send" /> is called more than once per
        ///     transaction.
        /// </param>
        public DiscreteCellStreamSink(Func<T, T, T> coalesce)
            : base(coalesce)
        {
        }

        /// <summary>
        ///     Return a reference to this <see cref="DiscreteCellStreamSink{T}" /> as a <see cref="StreamSink{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="DiscreteCellStreamSink{T}" /> as a <see cref="StreamSink{T}" />.</returns>
        public StreamSink<T> AsStreamSink() => this;
    }
}