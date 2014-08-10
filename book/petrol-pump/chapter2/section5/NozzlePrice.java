package chapter2.section5;

import pump.*;
import sodium.*;

public class NozzlePrice implements Pump
{
    public Outputs create(Inputs inputs)
    {
        Behavior<UpDown> nozzle1 = inputs.eNozzle1.hold(UpDown.DOWN);
        Behavior<UpDown> nozzle2 = inputs.eNozzle2.hold(UpDown.DOWN);
        Behavior<UpDown> nozzle3 = inputs.eNozzle3.hold(UpDown.DOWN);
        Lambda1<UpDown, String> ifLifted8 =
            u -> u.equals(UpDown.UP) ?  "8.8.8.8." : "";
        return new Outputs().setPriceLCD1(nozzle1.map(ifLifted8))
                            .setPriceLCD2(nozzle2.map(ifLifted8))
                            .setPriceLCD3(nozzle3.map(ifLifted8));
    }
}
