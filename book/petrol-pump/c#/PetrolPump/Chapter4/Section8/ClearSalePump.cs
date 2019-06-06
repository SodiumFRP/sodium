using PetrolPump.Chapter4.Section4;
using PetrolPump.Chapter4.Section7;
using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump.Chapter4.Section8
{
    public class ClearSalePump : IPump
    {
        public Outputs Create(Inputs inputs)
        {
            StreamLoop<Fuel> sStart = new StreamLoop<Fuel>();
            Fill fi = new Fill(
                inputs.SClearSale,
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
            return new Outputs()
                .SetDelivery(np.FuelFlowing.Map(
                    m =>
                        m.Equals(Maybe.Some(Fuel.One)) ? Delivery.Fast1 :
                            m.Equals(Maybe.Some(Fuel.Two)) ? Delivery.Fast2 :
                                m.Equals(Maybe.Some(Fuel.Three)) ? Delivery.Fast3 :
                                    Delivery.Off))
                .SetSaleCostLcd(fi.DollarsDelivered.Map(Formatters.FormatSaleCost))
                .SetSaleQuantityLcd(fi.LitersDelivered.Map(Formatters.FormatSaleQuantity))
                .SetPriceLcd1(ShowDollarsPump.PriceLcd(np.FillActive, fi.Price, Fuel.One, inputs))
                .SetPriceLcd2(ShowDollarsPump.PriceLcd(np.FillActive, fi.Price, Fuel.Two, inputs))
                .SetPriceLcd3(ShowDollarsPump.PriceLcd(np.FillActive, fi.Price, Fuel.Three, inputs))
                .SetBeep(np.SBeep)
                .SetSaleComplete(np.SSaleComplete);
        }
    }
}