using System.Collections.Generic;

namespace Sodium.Frp
{
    internal static class OperationalInternal
    {
        internal static Stream<T> UpdatesImpl<T>(Behavior<T> b) => TransactionInternal.Apply(
            (trans, _) => b.Updates().Coalesce(trans, (left, right) => right),
            false);

        internal static Stream<T> ValueImpl<T>(Behavior<T> b) => TransactionInternal.Apply((trans, _) => b.Value(trans), false);

        internal static Stream<T> DeferImpl<T>(Stream<T> s) => SplitImpl<T, T[]>(s.MapImpl(a => new[] { a }));

        internal static Stream<T> SplitImpl<T, TCollection>(Stream<TCollection> s)
            where TCollection : IEnumerable<T>
        {
            Stream<T> @out = new Stream<T>(s.KeepListenersAlive);
            IListener l1 = s.Listen(
                new Node<T>(),
                (trans, aa) =>
                {
                    int childIx = 0;
                    foreach (T a in aa)
                    {
                        trans.Split(childIx, trans1 => @out.Send(trans1, a));
                        childIx++;
                    }
                });
            return @out.UnsafeAttachListener(l1);
        }
    }
}