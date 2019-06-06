using System.Windows;
using Sodium.Frp;
using SWidgets;

namespace RedGreen
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            SButton red = new SButton { Content = "red", Width = 75 };
            SButton green = new SButton { Content = "green", Width = 75, Margin = new Thickness(5, 0, 0, 0) };
            Stream<string> sRed = red.SClicked.Map(_ => "red");
            Stream<string> sGreen = green.SClicked.Map(_ => "green");
            Stream<string> sColor = sRed.OrElse(sGreen);
            Cell<string> color = sColor.Hold(string.Empty);
            SLabel lbl = new SLabel(color) { Width = 75, Margin = new Thickness(5, 0, 0, 0) };

            this.Container.Children.Add(red);
            this.Container.Children.Add(green);
            this.Container.Children.Add(lbl);
        }
    }
}