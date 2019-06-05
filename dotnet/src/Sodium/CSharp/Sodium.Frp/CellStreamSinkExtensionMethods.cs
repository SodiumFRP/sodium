namespace Sodium.Frp
{
    public static class CellStreamSinkExtensionMethods
    {
        /// <summary>
        ///     Return a reference to this <see cref="CellStreamSink{T}" /> as a <see cref="StreamSink{T}" />.
        /// </summary>
        /// <typeparam name="T">The type of the cell stream sink.</typeparam>
        /// <param name="c">The cell stream sink.</param>
        /// <returns>A reference to this <see cref="CellStreamSink{T}" /> as a <see cref="StreamSink{T}" />.</returns>
        public static StreamSink<T> AsStreamSink<T>(this CellStreamSink<T> c) => c;
    }
}