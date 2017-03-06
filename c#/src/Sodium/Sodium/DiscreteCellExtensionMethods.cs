using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public static class DiscreteCellExtensionMethods
    {
        /// <summary>
        ///     Unwrap a discrete cell inside another discrete cell to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="cca">The discrete cell containing another discrete cell.</param>
        /// <returns>The unwrapped discrete cell.</returns>
        public static DiscreteCell<T> Switch<T>(this DiscreteCell<DiscreteCell<T>> cca) =>
            Transaction.Apply(trans => new DiscreteCell<T>(cca.Cell.Map(c => c.Cell).SwitchC()));

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