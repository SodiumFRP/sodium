using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section8
{
public class KeypadPump : IPump
{
    public Outputs Create(Inputs inputs) {
        Keypad ke = new Keypad(inputs.EKeypad, new Event<Unit>());
        return new Outputs()
            .SetPresetLcd(ke.Value.Map(v => Formatters.FormatSaleCost((double)v)))
            .SetBeep(ke.EBeep);
    }
}

}
