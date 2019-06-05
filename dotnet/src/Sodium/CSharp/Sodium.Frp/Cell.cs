using System;
using System.Runtime.CompilerServices;

namespace Sodium.Frp
{
    /// <summary>
    ///     Helper methods for creating a <see cref="Cell{T}" />.
    /// </summary>
    public static class Cell
    {
        /// <summary>
        ///     Creates a cell with a constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the cell.</typeparam>
        /// <param name="value">The value of the cell.</param>
        /// <returns>A cell with a constant value.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<T> Constant<T>(T value) => CellInternal.ConstantImpl(value);

        /// <summary>
        ///     Creates a cell with a lazy constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the cell.</typeparam>
        /// <param name="value">The lazy value of the cell.</param>
        /// <returns>A cell with a lazy constant value.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<T> ConstantLazy<T>(Lazy<T> value) => CellInternal.ConstantLazyImpl(value);

        /// <summary>
        ///     Construct a writable cell that uses the last value if <see cref="CellSinkExtensionMethods.Send{T}" /> is called
        ///     more than once per transaction.
        /// </summary>
        /// <typeparam name="T">The type of the value in the cell sink.</typeparam>
        /// <param name="initialValue">The initial value of the cell.</param>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static CellSink<T> CreateSink<T>(T initialValue) => CellInternal.CreateSinkImpl(initialValue);

        /// <summary>
        ///     Construct a writable cell that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="CellSinkExtensionMethods.Send{T}(CellSink{T}, T)" /> is called more than once per transaction.
        /// </summary>
        /// <typeparam name="T">The type of the value in the cell sink.</typeparam>
        /// <param name="initialValue">The initial value of the cell.</param>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="CellSinkExtensionMethods.Send{T}(CellSink{T}, T)" /> is called more
        ///     than once per transaction.
        /// </param>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static CellSink<T> CreateSink<T>(T initialValue, Func<T, T, T> coalesce) =>
            CellInternal.CreateSinkImpl(initialValue, coalesce);

        /// <summary>
        ///     Construct a writable cell stream sink that uses the last value if <see cref="CellSinkExtensionMethods.Send{T}" />
        ///     is called more than once per transaction.
        ///     This stream sink is meant to be turned into a <see cref="Cell{T}" /> through the use of
        ///     <see cref="StreamExtensionMethods.Hold{T}(Stream{T}, T)" />.
        /// </summary>
        /// <typeparam name="T">The type of the value in the cell stream sink.</typeparam>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static CellStreamSink<T> CreateStreamSink<T>() => CellInternal.CreateStreamSinkImpl<T>();

        /// <summary>
        ///     Construct a writable cell stream sink that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="StreamSinkExtensionMethods.Send{T}(StreamSink{T}, T)" /> is called more than once per transaction.
        ///     This stream sink is meant to be turned into a <see cref="Cell{T}" /> through the use of
        ///     <see cref="StreamExtensionMethods.Hold{T}(Stream{T}, T)" />.
        /// </summary>
        /// <typeparam name="T">The type of the value in the cell stream sink.</typeparam>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="StreamSinkExtensionMethods.Send{T}(StreamSink{T}, T)" /> is called more
        ///     than once per transaction.
        /// </param>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static CellStreamSink<T> CreateStreamSink<T>(Func<T, T, T> coalesce) =>
            CellInternal.CreateStreamSinkImpl(coalesce);

        /// <summary>
        ///     Creates a cell loop.
        /// </summary>
        /// <typeparam name="T">The type of values in the cell loop.</typeparam>
        /// <returns>The cell loop.</returns>
        public static CellLoop<T> CreateLoop<T>() => new CellLoop<T>();

        /// <summary>
        ///     Creates a helper to loop over a cell for the specified type.
        /// </summary>
        /// <typeparam name="T">The type of the cell to loop.</typeparam>
        /// <returns>A <see cref="CellLooper{T}"/> which should be used to complete the loop.</returns>
        [Pure]
        public static CellLooper<T> Loop<T>() => new CellLooper<T>();
    }

    /// <summary>
    ///     A helper to complete a loop over a cell.
    /// </summary>
    /// <typeparam name="T">The type of the cell being looped.</typeparam>
    public struct CellLooper<T>
    {
        /// <summary>
        ///     Loop a cell and return a value tuple containing the resulting cell and captures.
        /// </summary>
        /// <typeparam name="TCaptures">The type of the captures to return.</typeparam>
        /// <param name="f">A function which takes the cell loop and returns a value tuple containing the resulting cell and captures.</param>
        /// <returns>A value tuple containing the resulting cell and captures.</returns>
        [Pure]
        [MethodImpl(MethodImplOptions.NoInlining)]
        public (Cell<T> Cell, TCaptures Captures) WithCaptures<TCaptures>(
            Func<LoopedCell<T>, (Cell<T> Cell, TCaptures Captures)> f) =>
            TransactionInternal.Apply(
                (trans, _) =>
                {
                    LoopedCell<T> loop = new LoopedCell<T>();
                    (Cell<T> Cell, TCaptures Captures) result = f(loop);
                    loop.Loop(trans, result.Cell);
                    return result;
                },
                false);

        /// <summary>
        ///     Loop a cell and return the resulting cell.
        /// </summary>
        /// <param name="f">A function which takes the cell loop and returns the resulting cell.</param>
        /// <returns>The resulting cell.</returns>
        [Pure]
        public Cell<T> WithoutCaptures(Func<LoopedCell<T>, Cell<T>> f) =>
            this.WithCaptures(l => (Cell: f(l), Captures: UnitInternal.Value)).Cell;
    }
}