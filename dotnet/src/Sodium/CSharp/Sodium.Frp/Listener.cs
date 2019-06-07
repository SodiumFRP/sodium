using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Sodium.Frp
{
    public static class Listener
    {
        public static readonly IListener Empty = ListenerInternal.EmptyImpl;
        public static readonly IWeakListener EmptyWeak = ListenerInternal.EmptyWeakImpl;
        public static readonly IStrongListener EmptyStrong = ListenerInternal.EmptyStrongImpl;

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IListener CreateComposite(IReadOnlyList<IListener> listeners) =>
            ListenerInternal.CreateCompositeImpl(listeners);

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IWeakListener CreateWeakComposite(IReadOnlyList<IWeakListener> listeners) =>
            ListenerInternal.CreateWeakCompositeImpl(listeners);

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IStrongListener CreateStrongComposite(IReadOnlyList<IStrongListener> listeners) =>
            ListenerInternal.CreateStrongCompositeImpl(listeners);

        public static IListener Append(IListener listener1, IListener listener2) =>
            CreateComposite(new[] { listener1, listener2 });

        public static IWeakListener AppendWeak(IWeakListener listener1, IWeakListener listener2) =>
            CreateWeakComposite(new[] { listener1, listener2 });

        public static IStrongListener Append(IStrongListener listener1, IStrongListener listener2) =>
            CreateStrongComposite(new[] { listener1, listener2 });
    }
}
