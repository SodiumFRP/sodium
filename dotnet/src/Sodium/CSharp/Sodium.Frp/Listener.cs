using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Sodium.Frp
{
    public static class Listener
    {
        public static readonly IListener Empty = ListenerInternal.EmptyImpl;

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IListener CreateComposite(IReadOnlyList<IListener> listeners) => ListenerInternal.CreateCompositeImpl(listeners);

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IWeakListener CreateWeakComposite(IReadOnlyList<IWeakListener> listeners) =>
            ListenerInternal.CreateWeakCompositeImpl(listeners);

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IStrongListener CreateStrongComposite(IReadOnlyList<IStrongListener> listeners) =>
            ListenerInternal.CreateStrongCompositeImpl(listeners);

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IListener Append(IListener listener1, IListener listener2) =>
            ListenerInternal.AppendImpl(listener1, listener2);
    }
}
