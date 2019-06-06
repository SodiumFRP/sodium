using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump.Chapter4.Section4
{
    public class LifeCyclePump : IPump
    {
        public Outputs Create(Inputs inputs)
        {
            LifeCycle lc = new LifeCycle(inputs.SNozzle1, inputs.SNozzle2, inputs.SNozzle3);
            return new Outputs()
                .SetDelivery(lc.FillActive.Map(m =>
                    m.Equals(Maybe.Some(Fuel.One)) ? Delivery.Fast1 :
                        m.Equals(Maybe.Some(Fuel.Two)) ? Delivery.Fast2 :
                            m.Equals(Maybe.Some(Fuel.Three)) ? Delivery.Fast3 :
                                Delivery.Off))
                .SetSaleQuantityLcd(lc.FillActive.Map(m =>
                    m.Equals(Maybe.Some(Fuel.One)) ? "1" :
                        m.Equals(Maybe.Some(Fuel.Two)) ? "2" :
                            m.Equals(Maybe.Some(Fuel.Three)) ? "3" :
                                string.Empty));
        }
    }
}