using Sodium;

namespace PetrolPump.Chapter4.Section4
{
    public class LifeCyclePump : IPump
    {
        public Outputs Create(Inputs inputs)
        {
            LifeCycle lc = new LifeCycle(inputs.SNozzle1, inputs.SNozzle2, inputs.SNozzle3);
            return new Outputs()
                .SetDelivery(lc.FillActive.Map(m =>
                    m.Equals(Maybe.Just(Fuel.One)) ? Delivery.Fast1 :
                        m.Equals(Maybe.Just(Fuel.Two)) ? Delivery.Fast2 :
                            m.Equals(Maybe.Just(Fuel.Three)) ? Delivery.Fast3 :
                                Delivery.Off))
                .SetSaleQuantityLcd(lc.FillActive.Map(m =>
                    m.Equals(Maybe.Just(Fuel.One)) ? "1" :
                        m.Equals(Maybe.Just(Fuel.Two)) ? "2" :
                            m.Equals(Maybe.Just(Fuel.Three)) ? "3" :
                                string.Empty));
        }
    }
}