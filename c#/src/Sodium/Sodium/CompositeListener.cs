using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public class CompositeListener : IListener, IWeakListener
    {
        private readonly IReadOnlyList<IListener> listeners;

        public CompositeListener(IReadOnlyList<IListener> listeners)
        {
            this.listeners = listeners;
        }

        public void Unlisten()
        {
            foreach (IListener l in this.listeners)
            {
                l?.Unlisten();
            }
        }

        public IWeakListener GetWeakListener()
        {
            return new CompositeWeakListener(this.listeners.Select(l => l.GetWeakListener()).ToArray());
        }

        private class CompositeWeakListener : IWeakListener
        {
            private readonly IReadOnlyList<IWeakListener> weakListeners;

            public CompositeWeakListener(IReadOnlyList<IWeakListener> weakListeners)
            {
                this.weakListeners = weakListeners;
            }

            public void Unlisten()
            {
                foreach (IWeakListener l in this.weakListeners)
                {
                    l?.Unlisten();
                }
            }
        }
    }
}