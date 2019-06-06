using System.Collections.Generic;
using System.Windows.Controls;
using Fridgets;
using Sodium.Frp;

namespace Flow
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            this.Container.Children.Add(Transaction.Run(() =>
            {
                FrButton ok = new FrButton(Cell.Constant("OK"));
                FrButton cancel = new FrButton(Cell.Constant("Cancel"));
                IReadOnlyList<Fridget> fridgets = new[] { ok, cancel };
                Fridget dialog = new FrFlow(Orientation.Horizontal, fridgets);
                IListener lOk = ok.SClicked.Listen(_ => this.AddMessage("OK"));
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