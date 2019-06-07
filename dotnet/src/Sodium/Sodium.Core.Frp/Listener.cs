using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium.Frp
{
    /// <summary>
    ///     An interface representing an stream event listener which does not keep the stream from being garbage collected.
    ///     This may be used to stop listening on a stream by calling <see cref="Unlisten" />.
    /// </summary>
    public interface IListenerWithWeakReference
    {
        void Unlisten();
    }

    /// <summary>
    ///     An interface representing an stream event listener.  This may be used to stop listening on a stream by calling
    ///     <see cref="Unlisten" />.
    /// </summary>
    public interface IListener
    {
        void Unlisten();

        IListenerWithWeakReference GetListenerWithWeakReference();
    }

    /// <summary>
    ///     An interface representing an stream event listener.  This may be used to stop listening on a stream by calling
    ///     <see cref="IListener.Unlisten" />.
    /// </summary>
    public interface IStrongListener : IListener, IDisposable
    {
    }

    /// <summary>
    ///     An interface representing an stream event listener.  This may be used to stop listening on a stream by calling
    ///     <see cref="IListener.Unlisten" />.
    /// </summary>
    public interface IWeakListener : IListener
    {
    }

    internal interface IKeepListenersAlive
    {
        void KeepListenerAlive(IListener listener);
        void StopKeepingListenerAlive(IListener listener);
        void Use(IKeepListenersAlive childKeepListenersAlive);
    }

    internal static class ListenerInternal
    {
        internal static readonly IListener EmptyImpl = EmptyListener.Instance;
        internal static readonly IWeakListener EmptyWeakImpl = EmptyListener.Instance;
        internal static readonly IStrongListener EmptyStrongImpl = EmptyListener.Instance;

        internal static IListener CreateFromNodeAndTarget<T>(Node<T> node, Node<T>.Target target) =>
            new ActionListener(() => node.Unlink(target));

        internal static IListener CreateFromAction(Action unlisten) => new ActionListener(unlisten);

        internal static IListener CreateCompositeImpl<T>(IReadOnlyList<T> listeners) where T : IListener =>
            new CompositeListener<T>(listeners);

        internal static IWeakListener CreateWeakCompositeImpl(IReadOnlyList<IWeakListener> listeners) =>
            new CompositeWeakListener(listeners);

        internal static IStrongListener CreateStrongCompositeImpl(IReadOnlyList<IStrongListener> listeners) =>
            new CompositeStrongListener(listeners);

        private class EmptyListener : IStrongListener, IWeakListener, IListenerWithWeakReference
        {
            public static readonly EmptyListener Instance = new EmptyListener();

            private EmptyListener()
            {
            }

            public void Unlisten()
            {
            }

            public IListenerWithWeakReference GetListenerWithWeakReference() => this;

            public void Dispose()
            {
            }
        }

        /// <summary>
        ///     A listener which runs the specified action when it is disposed.
        /// </summary>
        private class ActionListener : IListener, IListenerWithWeakReference
        {
            private readonly Action unlisten;

            /// <summary>
            ///     Creates a listener which runs the specified action when it is disposed.
            /// </summary>
            /// <param name="unlisten">The action to run when this listener should stop listening.</param>
            internal ActionListener(Action unlisten) => this.unlisten = unlisten;

            public void Unlisten() => this.unlisten();
            public IListenerWithWeakReference GetListenerWithWeakReference() => this;
        }

        private class CompositeListener<T> : IListener, IListenerWithWeakReference
            where T : IListener
        {
            private readonly IReadOnlyList<T> listeners;

            public CompositeListener(IReadOnlyList<T> listeners) => this.listeners = listeners;

            public void Unlisten()
            {
                foreach (T l in this.listeners)
                {
                    l?.Unlisten();
                }
            }

            public IListenerWithWeakReference GetListenerWithWeakReference()
            {
                return new CompositeWeakListener(
                    this.listeners.Select(l => l.GetListenerWithWeakReference()).ToArray());
            }

            private class CompositeWeakListener : IListenerWithWeakReference
            {
                private readonly IReadOnlyList<IListenerWithWeakReference> weakListeners;

                public CompositeWeakListener(IReadOnlyList<IListenerWithWeakReference> weakListeners) =>
                    this.weakListeners = weakListeners;

                public void Unlisten()
                {
                    foreach (IListenerWithWeakReference l in this.weakListeners)
                    {
                        l?.Unlisten();
                    }
                }
            }
        }

        private class CompositeStrongListener : CompositeListener<IStrongListener>, IStrongListener
        {
            public CompositeStrongListener(IReadOnlyList<IStrongListener> listeners)
                : base(listeners)
            {
            }

            public void Dispose()
            {
                this.Unlisten();
            }
        }

        private class CompositeWeakListener : CompositeListener<IWeakListener>, IWeakListener
        {
            public CompositeWeakListener(IReadOnlyList<IWeakListener> listeners)
                : base(listeners)
            {
            }
        }
    }
}