using System;
using System.Collections.Generic;
using System.Windows.Controls;
using Sodium;

namespace SWidgets
{
    public class SButton : Button, IDisposable
    {
        private readonly IReadOnlyList<IListener> listeners;

        public SButton()
            : this(Cell.Constant(true))
        {
        }

        public SButton(Cell<bool> enabled)
        {
            StreamSink<Unit> sClickedSink = new StreamSink<Unit>();
            this.SClicked = sClickedSink;
            this.Click += (sender, args) => sClickedSink.Send(Unit.Value);

            // Set the initial value at the end of the transaction so it works with CellLoops.
            Transaction.Post(() => this.IsEnabled = enabled.Sample());

            // ReSharper disable once UseObjectOrCollectionInitializer
            List<IListener> listeners = new List<IListener>();
            listeners.Add(Operational.Updates(enabled).Listen(e => this.Dispatcher.InvokeIfNecessary(() => this.IsEnabled = e)));
            this.listeners = listeners;
        }

        public Stream<Unit> SClicked { get; }

        public void Dispose()
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