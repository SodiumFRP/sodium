using System;
using System.Collections.Concurrent;

namespace Sodium
{
    internal static class CoalesceHandler
    {
        internal static Action<Transaction, T> Create<T>(Func<T, T, T> f, Stream<T> @out)
        {
            bool accumValid = false;
            T accum = default(T);

            return (trans1, a) =>
            {
                if (accumValid)
                {
                    accum = f(accum, a);
                }
                else
                {
                    accum = a;
                    accumValid = true;

                    trans1.Prioritized(@out.Node, trans2 =>
                    {
                        // ReSharper disable once AccessToModifiedClosure
                        @out.Send(trans2, accum);
                        accumValid = false;
                        accum = default(T);
                    });
                }
            };
        }

        internal static Action<Transaction, T> CreateSafe<T>(Func<T, T, T> f, Stream<T> @out)
        {
            ConcurrentDictionary<Transaction, Action<Transaction, T>> handlersByTransaction = new ConcurrentDictionary<Transaction, Action<Transaction, T>>();
            return (t, a) => handlersByTransaction.GetOrAdd(t, Create(f, @out))(t, a);
        }
    }
}