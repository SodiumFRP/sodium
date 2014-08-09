package chapter2.section5;

import pump.*;
import sodium.*;

public class NozzlePrice implements Pump
{
    public Outputs create(Inputs inputs)
    {
        Lambda1<UpDown, String> ifLifted8 =
            u -> u.equals(UpDown.UP) ?  "8.8.8.8." : "";
        return new Outputs().setPriceLCD1(inputs.nozzle1.map(ifLifted8))
                            .setPriceLCD2(inputs.nozzle2.map(ifLifted8))
                            .setPriceLCD3(inputs.nozzle3.map(ifLifted8));
    }
}
