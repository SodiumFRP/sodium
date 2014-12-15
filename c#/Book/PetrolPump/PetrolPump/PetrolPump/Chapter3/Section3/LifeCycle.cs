using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section3
{
public class LifeCycle {
    public readonly Event<Fuel> EStart;
    public readonly Behavior<Optional<Fuel>> FillActive;
    public readonly Event<End> EEnd;

    public enum End { END }

    private static Event<Fuel> WhenLifted(Event<UpDown> eNozzle,
                                          Fuel nozzleFuel) 
    {
        return eNozzle.Filter(u => u == UpDown.UP)
                      .Map(u => nozzleFuel);
    }

    private static Event<End> WhenSetDown(Event<UpDown> eNozzle,
                Fuel nozzleFuel,
                Behavior<Optional<Fuel>> fillActive) 
    {
        return Event<End>.FilterOptional(
            eNozzle.Snapshot(fillActive,
                (u,f) => u == UpDown.DOWN &&
                         f.Equals(Optional<Fuel>.Of(nozzleFuel))
                                       ? Optional<End>.Of(End.END)
                                       : Optional<End>.Empty()));
    }

    public LifeCycle(Event<UpDown> eNozzle1,
                     Event<UpDown> eNozzle2,
                     Event<UpDown> eNozzle3) 
    {
        Event<Fuel> eLiftNozzle = WhenLifted(eNozzle1, Fuel.ONE).Merge(
                                  WhenLifted(eNozzle2, Fuel.TWO).Merge(
                                  WhenLifted(eNozzle3, Fuel.THREE)));
        BehaviorLoop<Optional<Fuel>> fillActive = new BehaviorLoop<Optional<Fuel>>();
        this.FillActive = fillActive;
        this.EStart = Event<Fuel>.FilterOptional(
            eLiftNozzle.Snapshot(fillActive, (newFuel, fillActive_) =>
                fillActive_.IsPresent ? Optional<Fuel>.Empty()
                                      : Optional<Fuel>.Of(newFuel)));
        this.EEnd = WhenSetDown(eNozzle1, Fuel.ONE, fillActive).Merge(
                    WhenSetDown(eNozzle2, Fuel.TWO, fillActive).Merge(
                    WhenSetDown(eNozzle3, Fuel.THREE, fillActive)));
      fillActive.Loop(
        EStart.Map(f => Optional<Fuel>.Of(f))
          .Merge(EEnd.Map(e => Optional<Fuel>.Empty()))
                  .Hold(Optional<Fuel>.Empty())
        );
    }
}

}
