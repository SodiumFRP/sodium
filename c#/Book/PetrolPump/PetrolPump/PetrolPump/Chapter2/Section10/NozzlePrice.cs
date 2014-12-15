using System;

using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter2.Section10
{

  public class NozzlePrice : IPump
  {
    public Outputs Create(Inputs inputs)
    {
      Behavior<UpDown> nozzle1 = inputs.ENozzle1.Hold(UpDown.DOWN);
      Behavior<UpDown> nozzle2 = inputs.ENozzle2.Hold(UpDown.DOWN);
      Behavior<UpDown> nozzle3 = inputs.ENozzle3.Hold(UpDown.DOWN);
      Func<UpDown, Double, String> ifLiftedPrice =
        (u, price) => u.Equals(UpDown.UP)
            ? Formatters.FormatPrice(price)
            : "";
      return new Outputs()
        .SetPriceLcd1(Behavior<UpDown>.Lift(ifLiftedPrice, nozzle1, inputs.Price1))
        .SetPriceLcd2(Behavior<UpDown>.Lift(ifLiftedPrice, nozzle2, inputs.Price2))
        .SetPriceLcd3(Behavior<UpDown>.Lift(ifLiftedPrice, nozzle3, inputs.Price3));
    }

  }
}
