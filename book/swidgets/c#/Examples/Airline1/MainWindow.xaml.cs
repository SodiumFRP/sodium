using Sodium.Frp;
using SWidgets;

namespace Airline1
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            SDateField dep = new SDateField();
            SDateField ret = new SDateField();
            Cell<bool> valid = dep.SelectedDate.Lift(ret.SelectedDate, (d, r) => d <= r);
            SButton ok = new SButton(valid) { Content = "OK", Width = 75 };

            this.DeparturePlaceholder.Children.Add(dep);
            this.ReturnPlaceholder.Children.Add(ret);
            this.ButtonPlaceholder.Children.Add(ok);
        }
    }
}