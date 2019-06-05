namespace Sodium.Frp
{
    public static class LoopedStreamExtensionMethods
    {
        /// <summary>
        ///     Return a reference to this <see cref="LoopedStream{T}" /> as a <see cref="Stream{T}" />.
        /// </summary>
        /// <typeparam name="T">The type of the looped stream.</typeparam>
        /// <param name="s">The looped stream.</param>
        /// <returns>A reference to this <see cref="LoopedStream{T}" /> as a <see cref="Stream{T}" />.</returns>
        public static Stream<T> AsStream<T>(this LoopedStream<T> s) => s;
    }
}