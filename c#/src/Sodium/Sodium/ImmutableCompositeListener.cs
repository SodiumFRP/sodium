using System.Collections.Generic;

namespace Sodium
{
    public class ImmutableCompositeListener : IListener
    {
        private readonly IReadOnlyList<IListener> listeners;

        public ImmutableCompositeListener(IReadOnlyList<IListener> listeners)
        {
            this.listeners = listeners;
        }

        public void Dispose()
        {
            this.Unlisten();
        }

        public void Unlisten()
        {
            foreach (IListener l in this.listeners)
            {
                using (l)
                {
                }
            }
        }
    }
}