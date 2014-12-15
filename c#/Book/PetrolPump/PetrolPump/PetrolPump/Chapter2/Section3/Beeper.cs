using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter2.Section3
{
  public class Beeper : IPump
  {
    public Outputs Create(Inputs inputs)
    {
      return new Outputs().SetBeep(inputs.EKeypad.Map(k => Unit.UNIT));
    }
  }
}
