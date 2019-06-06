using System.Windows;
using System.Windows.Controls;
using Sodium.Frp;

namespace SWidgets
{
    public class SSpinner : Grid
    {
        private SSpinner(int initialValue)
        {
            StreamLoop<int> sSetValue = new StreamLoop<int>();
            STextBox textField = new STextBox(sSetValue.Map(v => v.ToString()), initialValue.ToString()) { VerticalContentAlignment = VerticalAlignment.Center };
            this.Value = textField.Text.Map(t => int.TryParse(t, out int result) ? result : 0);
            SButton plus = new SButton { Content = "+", Width = 25 };
            SButton minus = new SButton { Content = "-", Width = 25 };

            this.RowDefinitions.Add(new RowDefinition { Height = GridLength.Auto });
            this.RowDefinitions.Add(new RowDefinition { Height = GridLength.Auto });
            this.ColumnDefinitions.Add(new ColumnDefinition { Width = new GridLength(1, GridUnitType.Star) });
            this.ColumnDefinitions.Add(new ColumnDefinition { Width = GridLength.Auto });

            SetRow(textField, 0);
            SetColumn(textField, 0);
            SetRowSpan(textField, 2);
            this.Children.Add(textField);

            SetRow(plus, 0);
            SetColumn(plus, 1);
            this.Children.Add(plus);

            SetRow(minus, 1);
            SetColumn(minus, 1);
            this.Children.Add(minus);

            Stream<int> sPlusDelta = plus.SClicked.Map(_ => 1);
            Stream<int> sMinusDelta = minus.SClicked.Map(_ => -1);
            Stream<int> sDelta = sPlusDelta.OrElse(sMinusDelta);
            sSetValue.Loop(sDelta.Snapshot(this.Value, (d, v) => v + d));
        }

        public Cell<int> Value { get; }

        public static SSpinner Create(int initialValue)
        {
            return Transaction.Run(() => new SSpinner(initialValue));
        }
    }
}