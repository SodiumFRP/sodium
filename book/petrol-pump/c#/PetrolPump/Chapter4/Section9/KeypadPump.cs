using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump.Chapter4.Section9
{
    public class KeypadPump : IPump
    {
        public Outputs Create(Inputs inputs)
        {
            Keypad ke = new Keypad(inputs.SKeypad, Stream.Never<Unit>());
            return new Outputs()
                .SetPresetLcd(ke.Value.Map(Formatters.FormatPresetAmount))
                .SetBeep(ke.SBeep);
        }
    }
}