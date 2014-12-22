package chapter2.section3;

import pump.*;
import sodium.*;

public class Beeper implements Pump {
    public Outputs create(Inputs inputs) {
        return new Outputs().setBeep(
            inputs.sKeypad.map(k -> Unit.UNIT)
        );
    }
}
