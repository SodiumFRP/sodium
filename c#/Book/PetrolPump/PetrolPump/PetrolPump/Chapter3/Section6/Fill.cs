using System;

using PetrolPump.Chapter3.Section5;
using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section6
{
public class Fill {
    public readonly Behavior<Double> Price;
    public readonly Behavior<Double> DollarsDelivered;
    public readonly Behavior<Double> LitersDelivered;

    public Fill(
            Event<Unit> eClearAccumulator, Event<int> eFuelPulses,
            Behavior<Double> calibration, Behavior<Double> price1,
            Behavior<Double> price2, Behavior<Double> price3,
            Event<Fuel> eStart) 
    {
        Price = CapturePrice(eStart, price1, price2, price3);
        LitersDelivered = AccumulatePulsesPump.Accumulate(
                eClearAccumulator, eFuelPulses, calibration);
        DollarsDelivered = Behavior<double>.Lift(
                (liters, price_) => liters * price_,
                LitersDelivered, Price);
    }

    public static Behavior<Double> CapturePrice(
            Event<Fuel> eStart,
            Behavior<Double> price1, Behavior<Double> price2,
            Behavior<Double> price3) 
    {
        Event<Optional<Double>> ePrice1 = eStart.Snapshot(price1,
            (f, p) => f == Fuel.ONE ? Optional<double>.Of(p)
                                    : Optional<double>.Empty());
        Event<Optional<Double>> ePrice2 = eStart.Snapshot(price2,
            (f, p) => f == Fuel.TWO ? Optional<double>.Of(p)
                                    : Optional<double>.Empty());
        Event<Optional<Double>> ePrice3 = eStart.Snapshot(price3,
            (f, p) => f == Fuel.THREE ? Optional<double>.Of(p)
                                      : Optional<double>.Empty());

        return Event<double>.FilterOptional(
            ePrice1.Merge(ePrice2.Merge(ePrice3))
        ).Hold(0.0);
    }
  }
}
