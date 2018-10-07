using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public static class CellExtensionMethods
    {
        /// <summary>
        ///     Unwrap a behavior inside a cell to give a time-varying behavior implementation.
        /// </summary>
        /// <typeparam name="T">The type of the behavior.</typeparam>
        /// <param name="cba">The cell containing a behavior.</param>
        /// <returns>The unwrapped behavior.</returns>
        public static Behavior<T> SwitchC<T>(this Cell<Behavior<T>> cba) => cba.Behavior.SwitchC();

        /// <summary>
        ///     Unwrap a cell inside another cell to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="cca">The cell containing another cell.</param>
        /// <returns>The unwrapped cell.</returns>
        public static Cell<T> SwitchC<T>(this Cell<Cell<T>> cca) => cca.Behavior.SwitchC();

        /// <summary>
        ///     Unwrap a stream inside a cell to give a time-varying stream implementation.
        ///     When the cell changes value, the output stream will fire the simultaneous firing (if one exists) from the
        ///     stream which the cell held at the beginning of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="csa">The cell containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        public static Stream<T> SwitchS<T>(this Cell<Stream<T>> csa) => csa.Behavior.SwitchS();

        /// <summary>
        ///     Lift a function into an enumerable of cells, so the returned cell always reflects the specified function applied to
        ///     the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The enumerable of cells.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the function applied to the input cells' values.</returns>
        public static Cell<TResult> Lift<T, TResult>(
            this IEnumerable<Cell<T>> c,
            Func<IReadOnlyList<T>, TResult> f) => c.ToArray().Lift(f);

        /// <summary>
        ///     Lift a function into a collection of cells, so the returned cell always reflects the specified function applied to
        ///     the
        ///     input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="c">The collection of cells.</param>
        /// <param name="f">The binary function to lift into the cells.</param>
        /// <returns>A cell containing values resulting from the function applied to the input cells' values.</returns>
        public static Cell<TResult> Lift<T, TResult>(
            this IReadOnlyCollection<Cell<T>> c,
            Func<IReadOnlyList<T>, TResult> f) => new Cell<TResult>(c.Select(i => i.Behavior).Lift(f));

        /// <summary>
        ///     Lift into an enumerable of cells, so the returned cell always reflects a list of the input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <param name="c">The enumerable of cells.</param>
        /// <returns>A cell containing a list of the input cells' values.</returns>
        public static Cell<IReadOnlyList<T>> Lift<T>(this IEnumerable<Cell<T>> c) => c.ToArray().Lift();

        /// <summary>
        ///     Lift into a collection of cells, so the returned cell always reflects a list of the input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <param name="c">The collection of cells.</param>
        /// <returns>A cell containing a list of the input cells' values.</returns>
        public static Cell<IReadOnlyList<T>> Lift<T>(this IReadOnlyCollection<Cell<T>> c) =>
            new Cell<IReadOnlyList<T>>(c.Select(i => i.Behavior).Lift());
    }
}