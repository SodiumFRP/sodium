using System;

using PetrolPump.Chapter3.Section6;
using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section10
{
  public class Preset
  {
    public readonly Behavior<Delivery> Delivery;
    public readonly Behavior<Boolean> KeypadActive;

    public enum Speed
    {
      FAST,
      SLOW,
      STOPPED
    };

    public Preset(
      Behavior<int> presetDollars,
      Fill fi,
      Behavior<Optional<Fuel>> fuelFlowing,
      Behavior<Boolean> fillActive)
    {
      Behavior<Speed> speed = Behavior<Speed>.Lift(
        (presetDollars_, price, dollarsDelivered, litersDelivered) =>
        {
          if (presetDollars_ == 0)
            return Speed.FAST;
          else
          {
            if (dollarsDelivered >= (double)presetDollars_)
              return Speed.STOPPED;
            double slowLiters =
              (double)presetDollars_ / price - 0.10;
            if (litersDelivered >= slowLiters)
              return Speed.SLOW;
            else
              return Speed.FAST;
          }
        },
        presetDollars,
        fi.Price,
        fi.DollarsDelivered,
        fi.LitersDelivered);
      Delivery = Behavior<Delivery>.Lift(
        (of, speed_) =>
          speed_ == Speed.FAST
            ? (
              of.Equals(Optional<Fuel>.Of(Fuel.ONE))
                ? Pump.Delivery.FAST1
                : of.Equals(Optional<Fuel>.Of(Fuel.TWO))
                  ? Pump.Delivery.FAST2
                  : of.Equals(Optional<Fuel>.Of(Fuel.THREE))
                    ? Pump.Delivery.FAST3
                    : Pump.Delivery.OFF
              )
            : speed_ == Speed.SLOW
              ? (
                of.Equals(Optional<Fuel>.Of(Fuel.ONE))
                  ? Pump.Delivery.SLOW1
                  : of.Equals(Optional<Fuel>.Of(Fuel.TWO))
                    ? Pump.Delivery.SLOW2
                    : of.Equals(Optional<Fuel>.Of(Fuel.THREE))
                      ? Pump.Delivery.SLOW3
                      : Pump.Delivery.OFF
                )
              : Pump.Delivery.OFF,
        fuelFlowing,
        speed);
      KeypadActive = Behavior<bool>.Lift(
        (of, speed_) =>
          !of.IsPresent || speed_ == Speed.FAST,
        fuelFlowing,
        speed);
    }
  }
}
