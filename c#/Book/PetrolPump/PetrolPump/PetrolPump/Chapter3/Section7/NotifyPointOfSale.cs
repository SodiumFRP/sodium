using System;

using PetrolPump.Chapter3.Section3;
using PetrolPump.Chapter3.Section6;
using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section7
{
  public class NotifyPointOfSale
  {
    public readonly Event<Fuel> EStart;
    public readonly Behavior<Optional<Fuel>> FillActive;
    public readonly Behavior<Optional<Fuel>> FuelFlowing;
    public readonly Event<LifeCycle.End> EEnd;
    public readonly Event<Unit> EBeep;
    public readonly Event<Sale> ESaleComplete;

    public NotifyPointOfSale(
      LifeCycle lc,
      Event<Unit> eClearSale,
      Fill fi)
    {
      Behavior<Boolean> locked = lc.EStart.Map(u => true).Merge(eClearSale.Map(u => false)).Hold(false);
      EStart = lc.EStart.Gate(locked.Map(l => !l));
      EEnd = lc.EEnd.Gate(locked);
      FuelFlowing = EStart.Map(f => Optional<Fuel>.Of(f))
        .Merge(EEnd.Map(f => Optional<Fuel>.Empty())).Hold(Optional<Fuel>.Empty());
      FillActive = EStart.Map(f => Optional<Fuel>.Of(f))
        .Merge(eClearSale.Map(f => Optional<Fuel>.Empty())).Hold(Optional<Fuel>.Empty());
      EBeep = eClearSale;
      ESaleComplete = Event<Sale>.FilterOptional(
        EEnd.Snapshot(
          Behavior<Sale>.Lift(
            (oFuel, price_, dollars, liters) => oFuel.IsPresent ? Optional<Sale>.Of(new Sale(oFuel.Get(), price_, dollars, liters)) : Optional<Sale>.Empty(),
            FuelFlowing,
            fi.Price,
            fi.DollarsDelivered,
            fi.LitersDelivered)));
    }
  }

}
