using System.Windows.Controls;
using Fridgets;
using Sodium;

namespace Button
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            this.Container.Children.Add(Transaction.Run(() =>
            {
                FrButton b = new FrButton(DiscreteCell.Constant("OK"));
                IListener l = b.SClicked.Listen(_ => this.AddMessage("clicked!"));
                return new FrView(this, b, l);
            }));
        }

        private void AddMessage(string message)
        {
            this.StackPanel.Children.Add(new TextBlock { Text = message });
            this.ScrollViewer.ScrollToBottom();
        }
    }
}