using System;
using Sodium.Frp;
using SWidgets;

namespace Airline2
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            this.InitializeComponent();

            SDateField dep = new SDateField();
            SDateField ret = new SDateField();
            Rule r1 = new Rule((d, r) => d <= r);
            Rule r2 = new Rule((d, r) => !Unlucky(d) && !Unlucky(r));
            Rule rule = r1.And(r2);
            Cell<bool> valid = rule.Reify(dep.SelectedDate, ret.SelectedDate);
            SButton ok = new SButton(valid) { Content = "OK", Width = 75 };

            this.DeparturePlaceholder.Children.Add(dep);
            this.ReturnPlaceholder.Children.Add(ret);
            this.ButtonPlaceholder.Children.Add(ok);
        }

        private static bool Unlucky(DateTime d)
        {
            int day = d.Day;
            return day == 4 || day == 14 || day == 24;
        }

        private class Rule
        {
            private readonly Func<DateTime, DateTime, bool> f;

            public Rule(Func<DateTime, DateTime, bool> f)
            {
                this.f = f;
            }

            public Cell<bool> Reify(Cell<DateTime> dep, Cell<DateTime> ret) => dep.Lift(ret, this.f);

            public Rule And(Rule other) => new Rule((d, r) => this.f(d, r) && other.f(d, r));
        }
    }
}