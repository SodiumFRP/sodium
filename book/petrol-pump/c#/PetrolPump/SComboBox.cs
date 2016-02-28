using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Controls;
using Sodium;

namespace PetrolPump
{
    public class SComboBox<T> : ComboBox
    {
        public SComboBox(IEnumerable<T> items, Func<T, string> getDisplayName)
        {
            this.ItemsSource = items.Select(item => new ComboBoxItem { Content = getDisplayName(item), Tag = item }).ToArray();
            this.SelectedIndex = 0;
            Func<T> getSelectedItem = () => (T)((ComboBoxItem)base.SelectedItem).Tag;
            CellSink<T> selectedItem = new CellSink<T>(getSelectedItem());
            this.SelectionChanged += async (sender, args) =>
            {
                T s = getSelectedItem();
                await Task.Run(() => selectedItem.Send(s));
            };
            this.SelectedItem = selectedItem;
        }

        public new Cell<T> SelectedItem { get; }
    }
}