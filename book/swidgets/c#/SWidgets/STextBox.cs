using System;
using System.Collections.Generic;
using System.Windows.Controls;
using Sodium.Frp;

namespace SWidgets
{
    public class STextBox : TextBox, IDisposable
    {
        private readonly Action disposeListeners;

        public STextBox(string initText)
            : this(Stream.Never<string>(), initText)
        {
        }

        public STextBox(string initText, Cell<bool> enabled)
            : this(Stream.Never<string>(), initText, enabled)
        {
        }

        public STextBox(Stream<string> setText, string initText)
            : this(setText, initText, Cell.Constant(true))
        {
        }

        public STextBox(Stream<string> setText, string initText, Cell<bool> enabled)
        {
            base.Text = initText;

            List<IListener> listeners = new List<IListener>();

            StreamSink<int> sDecrement = Stream.CreateSink<int>();
            Cell<bool> allow = setText.Map(_ => 1).OrElse(sDecrement).Accum(0, (b, d) => b + d).Map(b => b == 0);

            StreamSink<string> sUserChanges = Stream.CreateSink<string>();
            this.SUserChanges = sUserChanges;
            this.Text = sUserChanges.Gate(allow).OrElse(setText).Hold(initText);

            void TextChangedEventHandler(object sender, TextChangedEventArgs args)
            {
                string text = base.Text;
                this.Dispatcher.InvokeAsync(() => sUserChanges.Send(text));
            }

            this.TextChanged += TextChangedEventHandler;

            // Set the initial value at the end of the transaction so it works with CellLoops.
            Transaction.Post(() => this.Dispatcher.InvokeIfNecessary(() => this.IsEnabled = enabled.Sample()));

            listeners.Add(setText.Listen(t =>
            {
                this.Dispatcher.InvokeAsync(() =>
                {
                    this.TextChanged -= TextChangedEventHandler;
                    base.Text = t;
                    this.TextChanged += TextChangedEventHandler;
                    sDecrement.Send(-1);
                });
            }));

            listeners.Add(enabled.Updates().Listen(e => this.Dispatcher.InvokeIfNecessary(() => this.IsEnabled = e)));

            this.disposeListeners = () =>
            {
                foreach (IListener l in listeners)
                {
                    l.Unlisten();
                }
            };
        }

        public void Dispose()
        {
            this.disposeListeners();
        }

        public new Cell<string> Text { get; }
        public Stream<string> SUserChanges { get; }
    }
}