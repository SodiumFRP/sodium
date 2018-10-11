using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public class CompositeListener : CompositeListener<IListener>
    {
        public CompositeListener(IReadOnlyList<IListener> listeners)
            : base(listeners)
        {
        }
    }

    public class CompositeListener<T> : IListener, IListenerWithWeakReference
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
            return new CompositeWeakListener(this.listeners.Select(l => l.GetListenerWithWeakReference()).ToArray());
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
}