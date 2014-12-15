using PetrolPump.Chapter3.Section3;
using PetrolPump.Chapter3.Section6;
using PetrolPump.Chapter3.Section7;
using PetrolPump.Chapter3.Section8;
using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section10
{

  public class PresetAmountPump : IPump
  {
    public Outputs Create(Inputs inputs) 
    {
        EventLoop<Fuel> eStart = new EventLoop<Fuel>();
        Fill fi = new Fill(inputs.EClearSale.Map(u => Unit.UNIT),
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
        BehaviorLoop<bool> keypadActive = new BehaviorLoop<bool>();
        Keypad ke = new Keypad(inputs.EKeypad,
                               inputs.EClearSale,
                               keypadActive);
        Preset pr = new Preset(ke.Value,
                               fi,
                               np.FuelFlowing,
                               np.FillActive.Map(o => o.IsPresent));
        keypadActive.Loop(pr.KeypadActive);
        return new Outputs()
            .SetDelivery(pr.Delivery)
            .SetSaleCostLcd(fi.DollarsDelivered.Map(q => Formatters.FormatSaleCost(q)))
            .SetSaleQuantityLcd(fi.LitersDelivered.Map(q => Formatters.FormatSaleQuantity(q)))
            .SetPriceLcd1(ShowDollarsPump.PriceLCD(np.FillActive, fi.Price, Fuel.ONE, inputs))
            .SetPriceLcd2(ShowDollarsPump.PriceLCD(np.FillActive, fi.Price, Fuel.TWO, inputs))
            .SetPriceLcd3(ShowDollarsPump.PriceLCD(np.FillActive, fi.Price, Fuel.THREE, inputs))
            .SetSaleComplete(np.ESaleComplete)
            .SetPresetLcd(ke.Value.Map(v => Formatters.FormatSaleCost((double)v)))
            .SetBeep(np.EBeep.Merge(ke.EBeep));
    }
  }
}
