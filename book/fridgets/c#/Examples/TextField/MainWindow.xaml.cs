using System.Collections.Generic;
using System.Windows.Controls;
using Fridgets;
using Sodium.Frp;

namespace TextField
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            this.Container.Children.Add(Transaction.Run(() =>
            {
                FrTextField firstName = new FrTextField("Joe");
                FrTextField lastName = new FrTextField("Bloggs");
                FrButton ok = new FrButton(Cell.Constant("OK"));
                FrButton cancel = new FrButton(Cell.Constant("Cancel"));
                IReadOnlyList<Fridget> buttons = new[] { ok, cancel };
                Fridget buttonPanel = new FrFlow(Orientation.Horizontal, buttons);
                IReadOnlyList<Fridget> fridgets = new[] { buttonPanel, firstName, lastName };
                Fridget dialog = new FrFlow(Orientation.Vertical, fridgets);
                IListener lOk = ok.SClicked.Snapshot(firstName.Text, lastName.Text, (_, f, l) => f + " " + l).Listen(n => this.AddMessage("OK: " + n));
                IListener lCancel = cancel.SClicked.Listen(_ => this.AddMessage("Cancel"));
                return new FrView(this, dialog, Listener.CreateComposite(new[] { lOk, lCancel }));
            }));
        }

        private void AddMessage(string message)
        {
            this.StackPanel.Children.Add(new TextBlock { Text = message });
            this.ScrollViewer.ScrollToBottom();
        }
    }
}