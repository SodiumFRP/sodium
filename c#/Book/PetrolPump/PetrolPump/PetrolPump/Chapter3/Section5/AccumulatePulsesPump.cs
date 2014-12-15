using System;

using PetrolPump.Chapter3.Section3;
using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section5
{
public class AccumulatePulsesPump : IPump 
{
    public Outputs Create(Inputs inputs) 
    {
        LifeCycle lc = new LifeCycle(inputs.ENozzle1,
                                     inputs.ENozzle2,
                                     inputs.ENozzle3);
        Behavior<Double> litersDelivered =
                Accumulate(lc.EStart.Map(u => Unit.UNIT),
                           inputs.EFuelPulses,
                           inputs.Calibration);
        return new Outputs()
            .SetDelivery(lc.FillActive.Map(
                Of =>
                    Of.Equals(Optional<Fuel>.Of(Fuel.ONE))   ? Delivery.FAST1 :
                    Of.Equals(Optional<Fuel>.Of(Fuel.TWO))   ? Delivery.FAST2 :
                    Of.Equals(Optional<Fuel>.Of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF))
            .SetSaleQuantityLcd(litersDelivered.Map(
                    q => Formatters.FormatSaleQuantity(q)));
    }

    public static Behavior<double> Accumulate(
            Event<Unit> eClearAccumulator,
            Event<int> ePulses,
            Behavior<double> calibration) {
        BehaviorLoop<int> total = new BehaviorLoop<int>();
        total.Loop(ePulses.Snapshot(total,
            (pulses_, total_) => pulses_ + total_)
               .Merge(eClearAccumulator.Map(f => 0))
               .Hold(0));
        return Behavior<double>.Lift(
            (total_, calibration_) => total_ * calibration_,
            total, calibration);
    }
  }
}
