using Sodium.Frp;
using SWidgets;

namespace GameChat
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            Stream<string> sOnegai = this.OnegaiButton.SClicked.Map(_ => "Onegai shimasu");
            Stream<string> sThanks = this.ThanksButton.SClicked.Map(_ => "Thank you");
            Stream<string> sCanned = sOnegai.OrElse(sThanks);

            STextBox text = new STextBox(sCanned, string.Empty);
            this.TextPlaceholder.Children.Add(text);
        }
    }
}