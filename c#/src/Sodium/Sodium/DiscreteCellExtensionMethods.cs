using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public static class DiscreteCellExtensionMethods
    {
        /// <summary>
        ///     Unwrap a cell inside a discrete cell to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="cca">The discrete cell containing a cell.</param>
        /// <returns>The unwrapped cell.</returns>
        public static Cell<T> SwitchC<T>(this DiscreteCell<Cell<T>> cca) => cca.Cell.SwitchC();

        /// <summary>
        ///     Unwrap a discrete cell inside another discrete cell to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="cca">The discrete cell containing another discrete cell.</param>
        /// <returns>The unwrapped discrete cell.</returns>
        public static DiscreteCell<T> SwitchC<T>(this DiscreteCell<DiscreteCell<T>> cca) =>
            new DiscreteCell<T>(cca.Cell.Map(c => c.Cell).SwitchC());

        /// <summary>
        ///     Unwrap a stream inside a discrete cell to give a time-varying stream implementation.
        ///     When the discrete cell changes value, the output stream will fire the simultaneous firing (if one exists) from the
        ///     stream which the discrete cell held at the beginning of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="csa">The discrete cell containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        public static Stream<T> SwitchS<T>(this DiscreteCell<Stream<T>> csa) => csa.Cell.SwitchS();

        /// <summary>
        ///     Unwrap a stream inside a discrete cell to give a time-varying stream implementation.
        ///     When the discrete cell changes value, the output stream will fire the simultaneous firing (if one exists) from the
        ///     stream which the discrete cell will hold at the end of the transaction.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="csa">The discrete cell containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        public static Stream<T> SwitchEarlyS<T>(this DiscreteCell<Stream<T>> csa) => csa.Cell.SwitchEarlyS();

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
        public static DiscreteCell<TResult> Lift<T, TResult>(
            this IEnumerable<DiscreteCell<T>> c,
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
        public static DiscreteCell<TResult> Lift<T, TResult>(
            this IReadOnlyCollection<DiscreteCell<T>> c,
            Func<IReadOnlyList<T>, TResult> f) => new DiscreteCell<TResult>(c.Select(i => i.Cell).Lift(f));

        /// <summary>
        ///     Lift into an enumerable of cells, so the returned cell always reflects a list of the input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <param name="c">The enumerable of cells.</param>
        /// <returns>A discrete cell containing a list of the input cells' values.</returns>
        public static DiscreteCell<IReadOnlyList<T>> Lift<T>(this IEnumerable<DiscreteCell<T>> c) => c.ToArray().Lift();

        /// <summary>
        ///     Lift into a collection of cells, so the returned cell always reflects a list of the input cells' values.
        /// </summary>
        /// <typeparam name="T">The type of the cells.</typeparam>
        /// <param name="c">The collection of cells.</param>
        /// <returns>A discrete cell containing a list of the input cells' values.</returns>
        public static DiscreteCell<IReadOnlyList<T>> Lift<T>(this IReadOnlyCollection<DiscreteCell<T>> c) =>
            new DiscreteCell<IReadOnlyList<T>>(c.Select(i => i.Cell).Lift());
    }
}