namespace Sodium.Frp
{
    public static class LoopedCellExtensionMethods
    {
        /// <summary>
        ///     Return a reference to this <see cref="LoopedCell{T}" /> as a <see cref="Cell{T}" />.
        /// </summary>
        /// <typeparam name="T">The type of the looped cell.</typeparam>
        /// <param name="c">The looped cell.</param>
        /// <returns>A reference to this <see cref="LoopedCell{T}" /> as a <see cref="Cell{T}" />.</returns>
        public static Cell<T> AsCell<T>(this LoopedCell<T> c) => c;
    }
}