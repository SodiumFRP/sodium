using System;

namespace Sodium.Frp
{
    /// <summary>
    ///     A behavior that allows values to be pushed into it, acting as an interface between the world of I/O and the world of
    ///     FRP.  Code that exports instances of <see cref="BehaviorSink{T}" /> for read-only use should downcast to
    ///     <see cref="Behavior{T}" />.
    /// </summary>
    /// <typeparam name="T">The type of values in the behavior sink.</typeparam>
    public class BehaviorSink<T> : Behavior<T>
    {
        private readonly StreamSink<T> streamSink;

        internal BehaviorSink(T initialValue)
            : this(new StreamSink<T>((left, right) => right), initialValue)
        {
        }

        internal BehaviorSink(T initialValue, Func<T, T, T> coalesce)
            : this(new StreamSink<T>(coalesce), initialValue)
        {
        }

        private BehaviorSink(StreamSink<T> streamSink, T initialValue)
            : base(streamSink, initialValue) => this.streamSink = streamSink;

        internal void SendImpl(T a) => this.streamSink.SendImpl(a);
    }
}