using System.Linq;
using System.Windows;
using Sodium.Frp;
using SWidgets;

namespace Reverse
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            STextBox msg = new STextBox("Hello") { Width = 150 };
            Cell<string> reversed = msg.Text.Map(t => new string(t.Reverse().ToArray()));
            SLabel lbl = new SLabel(reversed) { Width = 150, Margin = new Thickness(5, 0, 0, 0) };

            this.Container.Children.Add(msg);
            this.Container.Children.Add(lbl);
        }
    }
}