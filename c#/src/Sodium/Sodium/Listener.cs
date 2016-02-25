using System;

namespace Sodium
{
    /// <summary>
    ///     A listener which runs the specified action when it is disposed.
    /// </summary>
    public class Listener : IListener
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

        public void Dispose()
        {
            this.Unlisten();
        }

        public void Unlisten()
        {
            this.unlisten();
        }
    }
}