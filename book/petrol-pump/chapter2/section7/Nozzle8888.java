package chapter2.section7;

import pump.*;
import sodium.*;

public class Nozzle8888 implements Pump
{
    public Outputs create(Inputs inputs)
    {
        Behavior<UpDown> nozzle1 = inputs.eNozzle1.hold(UpDown.DOWN);
        Behavior<UpDown> nozzle2 = inputs.eNozzle2.hold(UpDown.DOWN);
        Behavior<UpDown> nozzle3 = inputs.eNozzle3.hold(UpDown.DOWN);
        return new Outputs()
            .setPriceLCD1(nozzle1.map(u ->
                u.equals(UpDown.UP) ?  "8.8.8.8." : ""))
            .setPriceLCD2(nozzle2.map(u ->
                u.equals(UpDown.UP) ?  "8.8.8.8." : ""))
            .setPriceLCD3(nozzle3.map(u ->
                u.equals(UpDown.UP) ?  "8.8.8.8." : ""));
    }
}
