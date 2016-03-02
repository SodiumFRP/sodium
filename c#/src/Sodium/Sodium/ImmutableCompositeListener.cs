using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    public class ImmutableCompositeListener : IListener
    {
        private readonly IReadOnlyList<IListener> listeners;

        public ImmutableCompositeListener(IEnumerable<IListener> listeners)
        {
            this.listeners = (listeners ?? new IListener[0]).ToArray();
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