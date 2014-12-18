using System.Collections.Generic;
using System.Windows.Controls;

using Sodium;

namespace PetrolPump.Pump
{
  public class SComboBox<TA> : ComboBox
  {
    public SComboBox(IEnumerable<TA> aModel)
    {
      ItemsSource = aModel;

      BehaviorSink<TA> selectedItem = new BehaviorSink<TA>((TA)SelectedItem);
      SelectionChanged += (sender, args) => selectedItem.Send((TA)SelectedItem);
      SelectedItemBehavior = selectedItem;
    }

    public readonly Behavior<TA> SelectedItemBehavior;
  }
}
