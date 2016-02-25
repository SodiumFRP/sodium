using System;

namespace Sodium
{
    public static class CellExtensionMethods
    {
        /// <summary>
        ///     Unwrap a cell inside another cell to give a time-varying cell implementation.
        /// </summary>
        /// <typeparam name="T">The type of the cell.</typeparam>
        /// <param name="cca">The cell containing another cell.</param>
        /// <returns>The unwrapped cell.</returns>
        public static Cell<T> SwitchC<T>(this Cell<Cell<T>> cca)
        {
            return Transaction.Apply(trans1 =>
            {
                Lazy<T> za = cca.SampleLazy().Map(ca => ca.Sample());
                Stream<T> @out = new Stream<T>();
                IListener currentListener = null;
                Action<Transaction, Cell<T>> h = (trans2, ca) =>
                {
                    using (currentListener)
                    {
                    }

                    currentListener = ca.Value(trans2).Listen(@out.Node, trans2, (trans3, a) => @out.Send(trans3, a), false);
                };
                IListener l1 = cca.Value(trans1).Listen(@out.Node, trans1, h, false);
                return @out.UnsafeAddCleanup(l1).HoldLazy(za);
            });
        }

        /// <summary>
        ///     Unwrap a stream inside a cell to give a time-varying stream implementation.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="csa">The cell containing the stream.</param>
        /// <returns>The unwrapped stream.</returns>
        public static Stream<T> SwitchS<T>(this Cell<Stream<T>> csa)
        {
            return Transaction.Apply(trans1 =>
            {
                Stream<T> @out = new Stream<T>();
                IListener currentListener = csa.SampleNoTransaction().Listen(@out.Node, trans1, @out.Send, false);
                Action<Transaction, Stream<T>> h = (trans2, sa) =>
                {
                    trans2.Last(() =>
                    {
                        using (currentListener)
                        {
                        }

                        currentListener = sa.Listen(@out.Node, trans2, @out.Send, true);
                    });
                };
                IListener l1 = csa.Updates(trans1).Listen(@out.Node, trans1, h, false);
                return @out.UnsafeAddCleanup(l1);
            });
        }
    }
}