using System;
using System.Collections.Generic;
using System.Windows.Controls;
using Sodium.Frp;

namespace SWidgets
{
    public class SLabel : TextBlock, IDisposable
    {
        private readonly IReadOnlyList<IListener> listeners;

        public SLabel(Cell<string> text)
        {
            Action<string> setText = t => this.Dispatcher.InvokeIfNecessary(() => this.Text = t);

            // Set the initial value at the end of the transaction so it works with CellLoops.
            Transaction.Post(() => setText(text.Sample()));

            // ReSharper disable once UseObjectOrCollectionInitializer
            List<IListener> listeners = new List<IListener>();
            listeners.Add(text.Updates().Listen(setText));

            this.listeners = listeners;
        }

        public void Dispose()
        {
            foreach (IListener l in this.listeners)
            {
                l.Unlisten();
            }
        }
    }
}