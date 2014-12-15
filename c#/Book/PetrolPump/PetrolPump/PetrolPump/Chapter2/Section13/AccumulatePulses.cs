using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter2.Section13
{
public class AccumulatePulses : IPump
{
    public Outputs Create(Inputs inputs) {
        Behavior<UpDown> nozzle1 = inputs.ENozzle1.Hold(UpDown.DOWN);
        Behavior<UpDown> nozzle2 = inputs.ENozzle2.Hold(UpDown.DOWN);
        Behavior<UpDown> nozzle3 = inputs.ENozzle3.Hold(UpDown.DOWN);
        Behavior<UpDown> anyNozzle = Behavior<UpDown>.Lift((n1, n2, n3) =>
            n1 == UpDown.UP || n2 == UpDown.UP || n3 == UpDown.UP ? UpDown.UP : UpDown.DOWN, 
            nozzle1, nozzle2, nozzle3);
        Behavior<Double> litersDelivered = Accumulate(inputs.EFuelPulses, inputs.Calibration);
        return new Outputs()
            .SetDelivery(anyNozzle.Map(u => u == UpDown.UP ? Delivery.FAST1 : Delivery.OFF))
            .SetSaleQuantityLcd(litersDelivered.Map(q => Formatters.FormatSaleQuantity(q)));
    }

    private static Behavior<double> Accumulate(
            Event<int> ePulses, Behavior<double> calibration) {
        BehaviorLoop<int> total = new BehaviorLoop<int>();
        total.Loop(ePulses.Snapshot(total,(pulses_, total_) => pulses_ + total_).Hold(0));
        return Behavior<double>.Lift(
            (total_, calibration_) => total_ * calibration_,
            total, calibration);
    }
}
}
