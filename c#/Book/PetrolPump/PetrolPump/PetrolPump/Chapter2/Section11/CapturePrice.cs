using System;

using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter2.Section11
{
  public class CapturePrice : IPump
  {
    public Outputs Create(Inputs inputs)
    {
      return new Outputs()
        .SetPriceLcd1(perFuel(inputs.ENozzle1, inputs.Price1))
        .SetPriceLcd2(perFuel(inputs.ENozzle2, inputs.Price2))
        .SetPriceLcd3(perFuel(inputs.ENozzle3, inputs.Price3));
    }

    static Behavior<String> perFuel(
      Event<UpDown> eNozzle,
      Behavior<Double> price)
    {
      Behavior<Double> capPrice = eNozzle.Snapshot(price).Hold(0.0);
      Behavior<UpDown> nozzle = eNozzle.Hold(UpDown.DOWN);
      return Behavior<UpDown>.Lift(
        (u, price_) => u.Equals(UpDown.UP) ? Formatters.FormatPrice(price_) : "",
        nozzle,
        capPrice);
    }
  }
}