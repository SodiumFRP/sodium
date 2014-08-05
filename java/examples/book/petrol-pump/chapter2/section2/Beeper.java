package chapter2.section2;

import pump.*;
import sodium.*;

public class Beeper implements Pump
{
    public Pump.Outputs create(Pump.Inputs inputs)
    {
        return new Pump.Outputs().setBeep(
            inputs.eKeypad.map(k -> Unit.UNIT)
        );
    }
}
