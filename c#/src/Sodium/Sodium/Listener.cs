using System;

namespace Sodium
{
    /// <summary>
    ///     A listener which runs the specified action when it is disposed.
    /// </summary>
    public class Listener : IListener, IWeakListener
    {
        private readonly Action unlisten;

        /// <summary>
        ///     Creates a listener which runs the specified action when it is disposed.
        /// </summary>
        /// <param name="unlisten">The action to run when this listener should stop listening.</param>
        public Listener(Action unlisten)
        {
            this.unlisten = unlisten;
        }

        public void Unlisten()
        {
            this.unlisten();
        }

        public IWeakListener GetWeakListener()
        {
            return this;
        }
    }
}