using System;
using System.Runtime.CompilerServices;

namespace Sodium.Frp
{
    /// <summary>
    ///     Helper methods for creating a <see cref="Stream{T}" />.
    /// </summary>
    public static class Stream
    {
        /// <summary>
        ///     Creates a stream that never fires.
        /// </summary>
        /// <typeparam name="T">The type of the values that would be fired by the stream if it did fire values.</typeparam>
        /// <returns>A stream that never fires.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Never<T>() => StreamInternal.NeverImpl<T>();

        /// <summary>
        ///     Creates a StreamSink that throws an exception if <see cref="Stream{T}.Send" /> is called more than once per
        ///     transaction.
        /// </summary>
        /// <typeparam name="T">The type of values fired by the stream sink.</typeparam>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static StreamSink<T> CreateSink<T>() => StreamInternal.CreateSinkImpl<T>();

        /// <summary>
        ///     Construct a StreamSink that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="Stream{T}.Send" /> is called more than once per transaction.
        /// </summary>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="Stream{T}.Send" /> is called more than once per
        ///     transaction.
        /// </param>
        /// <typeparam name="T">The type of values fired by the stream sink.</typeparam>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static StreamSink<T> CreateSink<T>(Func<T, T, T> coalesce) => StreamInternal.CreateSinkImpl(coalesce);

        /// <summary>
        ///     Creates a <see cref="StreamLoop{T}" />.  This must be called and looped from within the same transaction.
        /// </summary>
        /// <typeparam name="T">The type of values in the stream loop.</typeparam>
        public static StreamLoop<T> CreateLoop<T>() => new StreamLoop<T>();

        /// <summary>
        ///     Creates a helper to loop over a stream for the specified type.
        /// </summary>
        /// <typeparam name="T">The type of the stream to loop.</typeparam>
        /// <returns>A <see cref="StreamLooper{T}"/> which should be used to complete the loop.</returns>
        [Pure]
        public static StreamLooper<T> Loop<T>() => new StreamLooper<T>();
    }

    /// <summary>
    ///     A helper to complete a loop over a stream.
    /// </summary>
    /// <typeparam name="T">The type of the stream being looped.</typeparam>
    public struct StreamLooper<T>
    {
        /// <summary>
        ///     Loop a stream and return a value tuple containing the resulting stream and captures.
        /// </summary>
        /// <typeparam name="TCaptures">The type of the captures to return.</typeparam>
        /// <param name="f">A function which takes the stream loop and returns a value tuple containing the resulting stream and captures.</param>
        /// <returns>A value tuple containing the resulting stream and captures.</returns>
        [Pure]
        [MethodImpl(MethodImplOptions.NoInlining)]
        public (Stream<T> Stream, TCaptures Captures) WithCaptures<TCaptures>(
            Func<LoopedStream<T>, (Stream<T> Stream, TCaptures Captures)> f) =>
            TransactionInternal.Apply(
                (trans, _) =>
                {
                    LoopedStream<T> loop = new LoopedStream<T>();
                    (Stream<T> Stream, TCaptures Captures) result = f(loop);
                    loop.Loop(trans, result.Stream);
                    return result;
                },
                false);

        /// <summary>
        ///     Loop a stream and return the resulting stream.
        /// </summary>
        /// <param name="f">A function which takes the stream loop and returns the resulting stream.</param>
        /// <returns>The resulting stream.</returns>
        [Pure]
        public Stream<T> WithoutCaptures(Func<LoopedStream<T>, Stream<T>> f) =>
            this.WithCaptures(l => (Stream: f(l), Captures: UnitInternal.Value)).Stream;
    }
}