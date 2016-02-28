using Sodium;

namespace PetrolPump.Chapter4.Section9
{
    public class KeypadPump : IPump
    {
        public Outputs Create(Inputs inputs)
        {
            Keypad ke = new Keypad(inputs.SKeypad, Stream.Never<Unit>());
            return new Outputs()
                .SetPresetLcd(ke.Value.Map(v => v.ToString("#0.00")))
                .SetBeep(ke.SBeep);
        }
    }
}