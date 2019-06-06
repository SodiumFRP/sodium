using Sodium.Frp;
using SWidgets;

namespace ClearField
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            SButton clear = new SButton { Content = "Clear", Width = 75 };
            Stream<string> sClearIt = clear.SClicked.Map(_ => string.Empty);
            STextBox text = new STextBox(sClearIt, "Hello") { Width = 100 };

            this.Container.Children.Add(text);
            this.Container.Children.Add(clear);
        }
    }
}