using System.Collections.Generic;

namespace Sodium
{
    public class CompositeStrongListener : CompositeListener<IStrongListener>, IStrongListener
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
}