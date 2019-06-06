using PetrolPump.Chapter4.Section4;
using PetrolPump.Chapter4.Section7;
using PetrolPump.Chapter4.Section8;
using PetrolPump.Chapter4.Section9;
using Sodium.Frp;

namespace PetrolPump.Chapter4.Section11
{
    public class PresetAmountPump : IPump
    {
        public Outputs Create(Inputs inputs)
        {
            StreamLoop<Fuel> sStart = new StreamLoop<Fuel>();
            Fill fi = new Fill(inputs.SClearSale,
                inputs.SFuelPulses, inputs.Calibration,
                inputs.Price1, inputs.Price2, inputs.Price3,
                sStart);
            NotifyPointOfSale np = new NotifyPointOfSale(
                new LifeCycle(inputs.SNozzle1,
                    inputs.SNozzle2,
                    inputs.SNozzle3),
                inputs.SClearSale,
                fi);
            sStart.Loop(np.SStart);
            CellLoop<bool> keypadActive = new CellLoop<bool>();
            Keypad ke = new Keypad(inputs.SKeypad,
                inputs.SClearSale,
                keypadActive);
            Preset pr = new Preset(ke.Value, fi, np.FuelFlowing);
            keypadActive.Loop(pr.KeypadActive);
            return new Outputs()
                .SetDelivery(pr.Delivery)
                .SetSaleCostLcd(fi.DollarsDelivered.Map(Formatters.FormatSaleCost))
                .SetSaleQuantityLcd(fi.LitersDelivered.Map(Formatters.FormatSaleQuantity))
                .SetPriceLcd1(ShowDollarsPump.PriceLcd(np.FillActive, fi.Price, Fuel.One, inputs))
                .SetPriceLcd2(ShowDollarsPump.PriceLcd(np.FillActive, fi.Price, Fuel.Two, inputs))
                .SetPriceLcd3(ShowDollarsPump.PriceLcd(np.FillActive, fi.Price, Fuel.Three, inputs))
                .SetSaleComplete(np.SSaleComplete)
                .SetPresetLcd(ke.Value.Map(Formatters.FormatPresetAmount))
                .SetBeep(np.SBeep.OrElse(ke.SBeep));
        }
    }
}