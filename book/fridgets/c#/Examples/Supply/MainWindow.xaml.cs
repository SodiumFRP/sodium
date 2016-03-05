using System.Windows.Controls;

namespace Supply
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            Fridgets.Supply a = new Fridgets.Supply();
            Fridgets.Supply b = a.Child1();
            Fridgets.Supply c = b.Child1();
            Fridgets.Supply bAgain = a.Child1();
            this.AddMessage("a = " + a.Get());
            this.AddMessage("b = " + b.Get());
            this.AddMessage("c = " + c.Get());
            this.AddMessage("bAgain = " + bAgain.Get());
        }

        private void AddMessage(string message)
        {
            this.StackPanel.Children.Add(new TextBlock { Text = message });
            this.ScrollViewer.ScrollToBottom();
        }
    }
}