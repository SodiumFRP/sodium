package chapter4.section9;

import pump.*;
import nz.sodium.*;
import java.util.Optional;

public class KeypadPump implements Pump
{
    public Outputs create(Inputs inputs) {
        Keypad ke = new Keypad(inputs.sKeypad, new Stream<Unit>());
        return new Outputs()
            .setPresetLCD(ke.value.map(v ->
                Formatters.formatSaleCost((double)v)))
            .setBeep(ke.sBeep);
    }
}

