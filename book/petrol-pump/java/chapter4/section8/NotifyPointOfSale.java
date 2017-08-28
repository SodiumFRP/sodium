package chapter4.section8;

import pump.*;
import chapter4.section4.LifeCycle;
import chapter4.section4.LifeCycle.End;
import chapter4.section7.Fill;
import nz.sodium.*;
import java.util.Optional;

public class NotifyPointOfSale {
    public final Stream<Fuel> sStart;
    public final Cell<Optional<Fuel>> fillActive;
    public final Cell<Optional<Fuel>> fuelFlowing;
    public final Stream<End> sEnd;
    public final Stream<Unit> sBeep;
    public final Stream<Sale> sSaleComplete;

    private enum Phase { IDLE, FILLING, POS };

    public NotifyPointOfSale(
             LifeCycle lc,
             Stream<Unit> sClearSale,
             Fill fi) {
        CellLoop<Phase> phase = new CellLoop<>();
        sStart = lc.sStart.gate(phase.map(p -> p == Phase.IDLE));
        sEnd   = lc.sEnd.gate(phase.map(p -> p == Phase.FILLING));
        phase.loop(
                sStart.map(u -> Phase.FILLING)
                .orElse(sEnd.map(u -> Phase.POS))
                .orElse(sClearSale.map(u -> Phase.IDLE))
                .hold(Phase.IDLE));
        fuelFlowing =
                sStart.map(f -> Optional.of(f)).orElse(
                sEnd.map(f -> Optional.empty())).hold(Optional.empty());
        fillActive =
             sStart.map(f -> Optional.of(f)).orElse(
             sClearSale.map(f -> Optional.empty())).hold(Optional.empty());
        sBeep = sClearSale;
        sSaleComplete = Stream.filterOptional(sEnd.snapshot(
            fuelFlowing.lift(fi.price, fi.dollarsDelivered,
            	                       fi.litersDelivered,
                (oFuel, price_, dollars, liters) ->
                    oFuel.isPresent() ? Optional.of(
                           new Sale(oFuel.get(), price_, dollars, liters))
                                      : Optional.empty())
		));
    }
}

