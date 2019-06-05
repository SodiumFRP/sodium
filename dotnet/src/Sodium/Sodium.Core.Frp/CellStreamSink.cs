using System;

namespace Sodium.Frp
{
    /// <summary>
    ///     A stream that allows values to be pushed into it which is meant to be used as the input update stream for a
    ///     <see cref="CellSink{T}" />.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell sink.</typeparam>
    public class CellStreamSink<T> : StreamSink<T>
    {
        internal CellStreamSink()
            : base((left, right) => right)
        {
        }

        internal CellStreamSink(Func<T, T, T> coalesce)
            : base(coalesce)
        {
        }
    }
}