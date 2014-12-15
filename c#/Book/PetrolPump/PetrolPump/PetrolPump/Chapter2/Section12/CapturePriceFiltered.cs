using System;

using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter2.Section12
{
  public class CapturePriceFiltered : IPump
  {
    public Outputs Create(Inputs inputs)
    {
      return new Outputs()
        .SetPriceLcd1(perFuel(inputs.ENozzle1, inputs.Price1))
        .SetPriceLcd2(perFuel(inputs.ENozzle2, inputs.Price2))
        .SetPriceLcd3(perFuel(inputs.ENozzle3, inputs.Price3));
    }

    enum StartFill
    {
      START_FILL
    };

    static Behavior<String> perFuel(
      Event<UpDown> eNozzle,
      Behavior<Double> price)
    {
      Event<StartFill> eStartFill = eNozzle.Filter(u => u == UpDown.UP) .Map(u => StartFill.START_FILL);
      Behavior<Double> capPrice = eStartFill.Snapshot(price).Hold(0.0);
      Behavior<UpDown> nozzle = eNozzle.Hold(UpDown.DOWN);
      return Behavior<UpDown>.Lift(
        (u, price_) => u.Equals(UpDown.UP) ? Formatters.FormatPrice(price_) : "",
        nozzle,
        capPrice);
    }
  }
}
