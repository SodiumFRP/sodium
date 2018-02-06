using System;

namespace Sodium
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

        public BehaviorSink(T initialValue)
            : this(new StreamSink<T>((left, right) => right), initialValue)
        {
        }

        public BehaviorSink(T initialValue, Func<T, T, T> coalesce)
            : this(new StreamSink<T>(coalesce), initialValue)
        {
        }

        private BehaviorSink(StreamSink<T> streamSink, T initialValue)
            : base(streamSink, initialValue) => this.streamSink = streamSink;

        /// <summary>
        ///     Send a value, modifying the value of the behavior.  This method may not be called from inside handlers registered with
        ///     <see cref="Stream{T}.Listen(Action{T})" /> or <see cref="Cell{T}.Listen(Action{T})" />.
        ///     An exception will be thrown, because sinks are for interfacing I/O to FRP only.  They are not meant to be used to
        ///     define new primitives.
        /// </summary>
        /// <param name="a">The value to send.</param>
        public void Send(T a) => this.streamSink.Send(a);

        /// <summary>
        ///     Return a reference to this <see cref="BehaviorSink{T}" /> as a <see cref="Behavior{T}" />.
        /// </summary>
        /// <returns>A reference to this <see cref="BehaviorSink{T}" /> as a <see cref="Behavior{T}" />.</returns>
        public Behavior<T> AsBehavior() => this;
    }
}