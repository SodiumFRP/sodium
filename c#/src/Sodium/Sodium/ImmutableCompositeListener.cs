using System.Collections.Generic;

namespace Sodium
{
    public class ImmutableCompositeListener : IListener, IWeakListener
    {
        private readonly IReadOnlyList<IListener> listeners;

        public ImmutableCompositeListener(IReadOnlyList<IListener> listeners)
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
            return this;
        }
    }
}