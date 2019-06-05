using System;

namespace Sodium.Frp
{
    /// <summary>
    ///     A cell that allows values to be pushed into it, acting as an interface between the world of I/O and the
    ///     world of
    ///     FRP.  Code that exports instances of <see cref="CellSink{T}" /> for read-only use should downcast to
    ///     <see cref="Cell{T}" />.
    /// </summary>
    /// <typeparam name="T">The type of values in the cell sink.</typeparam>
    public class CellSink<T> : Cell<T>
    {
        private readonly CellStreamSink<T> streamSink;

        internal CellSink(T initialValue)
            : this(new CellStreamSink<T>(), initialValue)
        {
        }

        internal CellSink(T initialValue, Func<T, T, T> coalesce)
            : this(new CellStreamSink<T>(coalesce), initialValue)
        {
        }

        private CellSink(CellStreamSink<T> streamSink, T initialValue)
            : this(streamSink, new Behavior<T>(streamSink, initialValue))
        {
        }

        private CellSink(CellStreamSink<T> streamSink, Behavior<T> behavior)
            : base(behavior) => this.streamSink = streamSink;

        internal void SendImpl(T a) => this.streamSink.SendImpl(a);
    }
}