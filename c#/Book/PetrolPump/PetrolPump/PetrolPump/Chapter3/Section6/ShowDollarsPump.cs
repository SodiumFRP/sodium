using System;

using PetrolPump.Chapter3.Section3;
using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section6
{
  public class ShowDollarsPump : IPump 
  {
    public Outputs Create(Inputs inputs) {
        LifeCycle lc = new LifeCycle(inputs.ENozzle1,
                                     inputs.ENozzle2,
                                     inputs.ENozzle3);
        Fill fi = new Fill(lc.EStart.Map(u => Unit.UNIT),
                           inputs.EFuelPulses, inputs.Calibration,
                           inputs.Price1, inputs.Price2, inputs.Price3,
                           lc.EStart);
        return new Outputs()
            .SetDelivery(lc.FillActive.Map(
                of =>
                    of.Equals(Optional<Fuel>.Of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.Equals(Optional<Fuel>.Of(Fuel.TWO)) ? Delivery.FAST2 :
                    of.Equals(Optional<Fuel>.Of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF))
            .SetSaleCostLcd(fi.DollarsDelivered.Map(
                q => Formatters.FormatSaleCost(q)))
            .SetSaleQuantityLcd(fi.LitersDelivered.Map(
                q => Formatters.FormatSaleQuantity(q)))
            .SetPriceLcd1(PriceLCD(lc.FillActive, fi.Price, Fuel.ONE,
                inputs))
            .SetPriceLcd2(PriceLCD(lc.FillActive, fi.Price, Fuel.TWO,
                inputs))
            .SetPriceLcd3(PriceLCD(lc.FillActive, fi.Price, Fuel.THREE,
                inputs));
    }

    public static Behavior<String> PriceLCD(
            Behavior<Optional<Fuel>> fillActive,
            Behavior<Double> fillPrice,
            Fuel fuel,
            Inputs inputs) {
        Behavior<Double> idlePrice;
        switch (fuel) {
            case Fuel.ONE:   idlePrice = inputs.Price1; break;
            case Fuel.TWO:   idlePrice = inputs.Price2; break;
            case Fuel.THREE: idlePrice = inputs.Price3; break;
            default:    idlePrice = null; break;
        }
        return Behavior<string>.Lift((oFuelSelected, fillPrice_, idlePrice_) =>
            oFuelSelected.IsPresent
                ? oFuelSelected.Get() == fuel
                                      ? Formatters.FormatPrice(fillPrice_)
                                      : ""
                : Formatters.FormatPrice(idlePrice_),
            fillActive,
            fillPrice,
            idlePrice);
    }
}

}
