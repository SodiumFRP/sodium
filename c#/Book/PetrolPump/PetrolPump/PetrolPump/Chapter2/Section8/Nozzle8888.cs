using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter2.Section8
{

public class Nozzle8888 : IPump
{
    public Outputs Create(Inputs inputs) {
        Behavior<UpDown> nozzle1 = inputs.ENozzle1.Hold(UpDown.DOWN);
        Behavior<UpDown> nozzle2 = inputs.ENozzle2.Hold(UpDown.DOWN);
        Behavior<UpDown> nozzle3 = inputs.ENozzle3.Hold(UpDown.DOWN);
        return new Outputs()
            .SetPriceLcd1(nozzle1.Map(u =>
                u.Equals(UpDown.UP) ?  "8.8.8.8." : ""))
            .SetPriceLcd2(nozzle2.Map(u =>
                u.Equals(UpDown.UP) ?  "8.8.8.8." : ""))
            .SetPriceLcd3(nozzle3.Map(u =>
                u.Equals(UpDown.UP) ?  "8.8.8.8." : ""));
    }
}
}
