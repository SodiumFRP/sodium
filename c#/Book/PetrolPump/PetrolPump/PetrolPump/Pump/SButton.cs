using System;
using System.Windows.Controls;

using Sodium;

namespace PetrolPump.Pump
{
  public class SButton : Button
  {
    public SButton(String label)
    {
      Content = label;
      EventSink<Unit> eClickedSink = new EventSink<Unit>();
      EClicked = eClickedSink;
      Click += (sender, args) => { eClickedSink.Send(Unit.UNIT); };
    }

    public readonly Event<Unit> EClicked;
  }
}
