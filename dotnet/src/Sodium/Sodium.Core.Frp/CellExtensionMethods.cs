using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium.Frp
{
    internal static class CellExtensionMethodsInternal
    {
        internal static Behavior<T> SwitchBImpl<T, T2>(this Cell<T2> cba)
            where T2 : Behavior<T> =>
            cba.BehaviorImpl.SwitchBImpl<T, T2>();

        internal static Cell<T> SwitchCImpl<T, T2>(this Cell<T2> cca)
            where T2 : Cell<T> =>
            cca.BehaviorImpl.SwitchCImpl<T, T2>();

        internal static Stream<T> SwitchSImpl<T, T2>(this Cell<T2> csa)
            where T2 : Stream<T> =>
            csa.BehaviorImpl.SwitchSImpl<T, T2>();

        internal static Cell<TResult> LiftCellsImpl<T, T2, TResult>(
            this IEnumerable<T2> c,
            Func<IReadOnlyList<T>, TResult> f)
            where T2 : Cell<T> =>
            new Cell<TResult>(c.Select(i => i.BehaviorImpl).ToArray().LiftBehaviorsImpl(f));

        internal static Cell<TResult> LiftCellsImpl<T, T2, TResult>(
            this IReadOnlyCollection<T2> c,
            Func<IReadOnlyList<T>, TResult> f)
            where T2 : Cell<T> =>
            c.AsEnumerable().LiftCellsImpl(f);
    }
}