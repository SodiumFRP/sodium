using Sodium.Frp;
using SWidgets;

namespace Add
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            STextBox txtA = new STextBox("5") { Width = 100 };
            STextBox txtB = new STextBox("10") { Width = 100 };

            Cell<int> a = txtA.Text.Map(ParseInt);
            Cell<int> b = txtB.Text.Map(ParseInt);
            Cell<int> sum = a.Lift(b, (x, y) => x + y);

            SLabel lblSum = new SLabel(sum.Map(i => i.ToString()));

            this.Container.Children.Add(txtA);
            this.Container.Children.Add(txtB);
            this.Container.Children.Add(lblSum);
        }

        private static int ParseInt(string t)
        {
            int result;
            if (int.TryParse(t, out result))
            {
                return result;
            }

            return 0;
        }
    }
}