using System;

using Sodium;

namespace PetrolPump.Pump
{
  public class Inputs
  {
    public Inputs(
      Event<UpDown> eNozzle1,
      Event<UpDown> eNozzle2,
      Event<UpDown> eNozzle3,
      Event<Key> eKeypad,
      Event<int> eFuelPulses,
      Behavior<Double> calibration,
      Behavior<Double> price1,
      Behavior<Double> price2,
      Behavior<Double> price3,
      Event<Unit> eClearSale)
    {
      ENozzle1 = eNozzle1;
      ENozzle2 = eNozzle2;
      ENozzle3 = eNozzle3;
      EKeypad = eKeypad;
      EFuelPulses = eFuelPulses;
      Calibration = calibration;
      Price1 = price1;
      Price2 = price2;
      Price3 = price3;
      EClearSale = eClearSale;
    }

    public readonly Event<UpDown> ENozzle1;
    public readonly Event<UpDown> ENozzle2;
    public readonly Event<UpDown> ENozzle3;
    public readonly Event<Key> EKeypad;
    public readonly Event<int> EFuelPulses;
    public readonly Behavior<Double> Calibration;
    public readonly Behavior<Double> Price1;
    public readonly Behavior<Double> Price2;
    public readonly Behavior<Double> Price3;
    public readonly Event<Unit> EClearSale;
  }
}