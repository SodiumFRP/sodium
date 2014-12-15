using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section3
{
public class LifeCyclePump : IPump 
{
    public Outputs Create(Inputs inputs) 
    {
        LifeCycle lc = new LifeCycle(inputs.ENozzle1,
                                     inputs.ENozzle2,
                                     inputs.ENozzle3);
        return new Outputs()
            .SetDelivery(lc.FillActive.Map(
                of =>
                    of.Equals(Optional<Fuel>.Of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.Equals(Optional<Fuel>.Of(Fuel.TWO))   ? Delivery.FAST2 :
                    of.Equals(Optional<Fuel>.Of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF))
            .SetSaleQuantityLcd(lc.FillActive.Map(
                of =>
                    of.Equals(Optional<Fuel>.Of(Fuel.ONE))   ? "1" :
                    of.Equals(Optional<Fuel>.Of(Fuel.TWO))   ? "2" :
                    of.Equals(Optional<Fuel>.Of(Fuel.THREE)) ? "3" : ""));
    }
}

}
