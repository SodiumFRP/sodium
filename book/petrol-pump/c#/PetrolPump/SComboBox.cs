using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Controls;
using Sodium.Frp;

namespace PetrolPump
{
    public class SComboBox<T> : ComboBox
    {
        public SComboBox(IEnumerable<T> items, Func<T, string> getDisplayName)
        {
            this.ItemsSource = items.Select(item => new ComboBoxItem { Content = getDisplayName(item), Tag = item }).ToArray();
            this.SelectedIndex = 0;
            T GetSelectedItem() => (T) ((ComboBoxItem) base.SelectedItem).Tag;
            CellSink<T> selectedItem = Cell.CreateSink(GetSelectedItem());
            this.SelectionChanged += async (sender, args) =>
            {
                T s = GetSelectedItem();
                await Task.Run(() => selectedItem.Send(s));
            };
            this.SelectedItem = selectedItem;
        }

        public new Cell<T> SelectedItem { get; }
    }
}