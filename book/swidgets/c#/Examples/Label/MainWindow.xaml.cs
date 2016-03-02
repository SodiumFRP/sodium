using System.Windows;
using SWidgets;

namespace Label
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            STextBox msg = new STextBox("Hello") { Width = 150 };
            SLabel lbl = new SLabel(msg.Text) { Width = 150, Margin = new Thickness(5, 0, 0, 0) };

            this.Container.Children.Add(msg);
            this.Container.Children.Add(lbl);
        }
    }
}