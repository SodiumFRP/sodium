using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Controls;
using Sodium.Frp;
using Sodium.Functional;

namespace SWidgets
{
    public class SDateField : WrapPanel
    {
        private static readonly Dictionary<string, int> Months = new Dictionary<string, int>
        {
            { "Jan", 1 },
            { "Feb", 2 },
            { "Mar", 3 },
            { "Apr", 4 },
            { "May", 5 },
            { "Jun", 6 },
            { "Jul", 7 },
            { "Aug", 8 },
            { "Sep", 9 },
            { "Oct", 10 },
            { "Nov", 11 },
            { "Dec", 12 }
        };

        public SDateField()
        {
            DateTime now = DateTime.Now;
            SComboBox<int> year = new SComboBox<int>(Maybe.Some(now.Year), Enumerable.Range(now.Year - 10, 21));
            SComboBox<string> month = new SComboBox<string>(Maybe.Some(Months.Single(p => p.Value == now.Month).Key), Months.Keys);
            SComboBox<int> day = new SComboBox<int>(Maybe.Some(now.Day), Enumerable.Range(1, 31));

            this.Children.Add(year);
            this.Children.Add(month);
            this.Children.Add(day);

            Cell<Maybe<int>> monthIndex = month.SelectedItem.Map(m => m.Match(s => Maybe.Some(Months[s]), () => Maybe.None));
            this.SelectedDate = year.SelectedItem.Lift(monthIndex, day.SelectedItem,
                (my, mm, md) =>
                    from y in my
                    from m in mm
                    from d in md
                    select new DateTime(y, m, d)).Map(m => m.Match(v => v, () => DateTime.Now));
        }

        public Cell<DateTime> SelectedDate;
    }
}