using System;

namespace Sodium
{
    /// <summary>
    ///     A listener which runs the specified action when it is disposed.
    /// </summary>
    public class Listener : IListener, IListenerWithWeakReference
    {
        private readonly Action unlisten;

        /// <summary>
        ///     Creates a listener which runs the specified action when it is disposed.
        /// </summary>
        /// <param name="unlisten">The action to run when this listener should stop listening.</param>
        private Listener(Action unlisten) => this.unlisten = unlisten;

        internal static IListener Create<T>(Node<T> node, Node<T>.Target target) =>
            new Listener(() => node.Unlink(target));

        internal static IListener Create(Action unlisten) => new Listener(unlisten);

        public void Unlisten() => this.unlisten();
        public IListenerWithWeakReference GetListenerWithWeakReference() => this;
    }
}