using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public static class DiscreteCellExtensionMethods
    {
        /// <summary>
        ///     Convert the given stream into a discrete cell with the specified initial value.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="stream">The stream which will provide updates to the discrete cell.</param>
        /// <param name="initialValue">The initial value of the discrete cell.</param>
        /// <returns>A discrete cell with initial value <see cref="initialValue"/> receiving updates from <see cref="stream"/>.</returns>
        public static DiscreteCell<T> ToDiscreteCell<T>(this Stream<T> stream, T initialValue) => DiscreteCell.Create(stream, initialValue);

        /// <summary>
        ///     Unwrap a discrete cell inside another discrete cell to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="cca">The discrete cell containing another discrete cell.</param>
        /// <returns>The unwrapped discrete cell.</returns>
        public static DiscreteCell<T> Switch<T>(this DiscreteCell<DiscreteCell<T>> cca) =>
            Transaction.Run(
                () => DiscreteCell.Create(
                    cca.Updates.Map(v => v.Cell.Sample())
                        .Merge(cca.Cell.Map(v => v.Updates).SwitchEarlyS(), (o, n) => n)
                        .Snapshot(cca.Cell.Map(v => v.Cell).SwitchC(), (n, o) => n),
                    cca.Cell.Sample().Cell.Sample()));

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
            Transaction.Run(() =>
            {
                IReadOnlyList<T> initialValue = c.Select(o => o.Cell.Sample()).ToArray();
                Stream<IReadOnlyList<T>> stream =
                    c.Select((o, i) => o.Updates.Map(v => new[] { (Func<T[], T[]>)(vv =>
                    {
                        vv[i] = v;
                        return vv;
                    }) }.AsEnumerable())).Merge((x, y) => x.Concat(y))
                        .Snapshot(c.Select(o => o.Cell).LiftToArray(), (ff, x) => ff.Aggregate(x, (v, f) => f(v))).Map<IReadOnlyList<T>>(v => v);
                return DiscreteCell.Create(stream, initialValue);
            });
    }
}