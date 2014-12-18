using System;
using System.Windows.Controls;

using Sodium;

namespace PetrolPump.Pump
{
  public class STextBox : TextBox
  {
    public STextBox(String initText, int width)
    {
      Text = initText;
      Width = width;

      BehaviorSink<String> text = new BehaviorSink<String>(initText);
      TextBehavior = text;
      TextChanged += (sender, args) => text.Send(this.Text);
    }

    public readonly Behavior<String> TextBehavior;
  }
}
