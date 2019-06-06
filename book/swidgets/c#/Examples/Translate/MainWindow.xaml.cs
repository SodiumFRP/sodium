using System.Text.RegularExpressions;
using Sodium.Frp;
using SWidgets;

namespace Translate
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            STextBox english = new STextBox("I like FRP") { Width = 150 };
            this.TextBoxPlaceholder.Children.Add(english);

            Stream<string> sLatin = this.TranslateButton.SClicked.Snapshot(english.Text, (u, t) => Regex.Replace(t.Trim(), " |$", "us "));
            Cell<string> latin = sLatin.Hold(string.Empty);
            SLabel lblLatin = new SLabel(latin);
            this.TextPlaceholder.Children.Add(lblLatin);
        }
    }
}