using System.Collections.Generic;

namespace Sodium
{
    public class CompositeListener : IListener
    {
        private readonly List<IListener> listeners;

        public CompositeListener()
            : this(null)
        {
        }

        public CompositeListener(IEnumerable<IListener> listeners)
        {
            this.listeners = new List<IListener>(listeners ?? new IListener[0]);
        }

        public void Add(IListener l)
        {
            this.listeners.Add(l);
        }

        public void AddRange(IEnumerable<IListener> l)
        {
            this.listeners.AddRange(l);
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