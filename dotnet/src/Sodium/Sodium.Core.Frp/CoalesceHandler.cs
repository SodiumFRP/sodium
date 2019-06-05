using System;

namespace Sodium.Frp
{
    internal static class CoalesceHandler
    {
        internal static Action<TransactionInternal, T> Create<T>(Func<T, T, T> f, Stream<T> @out)
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

                    trans1.Prioritized(
                        @out.Node,
                        trans2 =>
                        {
                            // ReSharper disable once AccessToModifiedClosure
                            @out.Send(trans2, accum);
                            accumValid = false;
                            accum = default(T);
                        });
                }
            };
        }
    }
}