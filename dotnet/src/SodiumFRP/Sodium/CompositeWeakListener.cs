using System.Collections.Generic;

namespace Sodium
{
    public class CompositeWeakListener : CompositeListener<IWeakListener>, IWeakListener
    {
        public CompositeWeakListener(IReadOnlyList<IWeakListener> listeners)
            : base(listeners)
        {
        }
    }
}