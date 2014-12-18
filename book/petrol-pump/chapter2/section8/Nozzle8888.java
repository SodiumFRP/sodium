package chapter2.section8;

import pump.*;
import sodium.*;

public class Nozzle8888 implements Pump
{
    public Outputs create(Inputs inputs) {
        Cell<UpDown> nozzle1 = inputs.sNozzle1.hold(UpDown.DOWN);
        Cell<UpDown> nozzle2 = inputs.sNozzle2.hold(UpDown.DOWN);
        Cell<UpDown> nozzle3 = inputs.sNozzle3.hold(UpDown.DOWN);
        return new Outputs()
            .setPriceLCD1(nozzle1.map(u ->
                u.equals(UpDown.UP) ?  "8.8.8.8." : ""))
            .setPriceLCD2(nozzle2.map(u ->
                u.equals(UpDown.UP) ?  "8.8.8.8." : ""))
            .setPriceLCD3(nozzle3.map(u ->
                u.equals(UpDown.UP) ?  "8.8.8.8." : ""));
    }
}
