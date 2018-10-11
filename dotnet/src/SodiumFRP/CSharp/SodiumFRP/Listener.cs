using System.Collections.Generic;

namespace SodiumFRP
{
    public static class Listener
    {
        public static readonly IListener Empty = ListenerInternal.EmptyImpl;

        public static IListener CreateComposite(IReadOnlyList<IListener> listeners) => ListenerInternal.CreateCompositeImpl(listeners);

        public static IWeakListener CreateWeakComposite(IReadOnlyList<IWeakListener> listeners) =>
            ListenerInternal.CreateWeakCompositeImpl(listeners);

        public static IStrongListener CreateStrongComposite(IReadOnlyList<IStrongListener> listeners) =>
            ListenerInternal.CreateStrongCompositeImpl(listeners);

        public static IListener Append(IListener listener1, IListener listener2) =>
            ListenerInternal.AppendImpl(listener1, listener2);
    }
}
