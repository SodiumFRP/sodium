using PetrolPump.Chapter3.Section3;
using PetrolPump.Chapter3.Section6;
using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section7
{
  public class ClearSalePump : IPump 
  {
    public Outputs Create(Inputs inputs) 
    {
        EventLoop<Fuel> eStart = new EventLoop<Fuel>();
        Fill fi = new Fill(
                          inputs.EClearSale.Map(u => Unit.UNIT),
                          inputs.EFuelPulses, inputs.Calibration,
                          inputs.Price1, inputs.Price2, inputs.Price3,
                          eStart);
        NotifyPointOfSale np = new NotifyPointOfSale(
                new LifeCycle(inputs.ENozzle1,
                              inputs.ENozzle2,
                              inputs.ENozzle3),
                inputs.EClearSale,
                fi);
        eStart.Loop(np.EStart);
        return new Outputs()
            .SetDelivery(np.FuelFlowing.Map(
                of =>
                    of.Equals(Optional<Fuel>.Of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.Equals(Optional<Fuel>.Of(Fuel.TWO))   ? Delivery.FAST2 :
                    of.Equals(Optional<Fuel>.Of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF))
            .SetSaleCostLcd(fi.DollarsDelivered.Map(
                    q => Formatters.FormatSaleCost(q)))
            .SetSaleQuantityLcd(fi.LitersDelivered.Map(
                    q => Formatters.FormatSaleQuantity(q)))
            .SetPriceLcd1(ShowDollarsPump.PriceLCD(np.FillActive, fi.Price,
                    Fuel.ONE, inputs))
            .SetPriceLcd2(ShowDollarsPump.PriceLCD(np.FillActive, fi.Price,
                    Fuel.TWO, inputs))
            .SetPriceLcd3(ShowDollarsPump.PriceLCD(np.FillActive, fi.Price,
                    Fuel.THREE, inputs))
            .SetBeep(np.EBeep)
            .SetSaleComplete(np.ESaleComplete);
    }
  }
}
