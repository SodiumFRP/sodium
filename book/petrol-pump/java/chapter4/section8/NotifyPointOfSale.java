package chapter4.section8;

import pump.*;
import chapter4.section4.LifeCycle;
import chapter4.section4.LifeCycle.End;
import chapter4.section7.Fill;
import sodium.*;
import java.util.Optional;

public class NotifyPointOfSale {
    public final Stream<Fuel> sStart;
    public final Cell<Optional<Fuel>> fillActive;
    public final Cell<Optional<Fuel>> fuelFlowing;
    public final Stream<End> sEnd;
    public final Stream<Unit> sBeep;
    public final Stream<Sale> sSaleComplete;

    public NotifyPointOfSale(
             LifeCycle lc,
             Stream<Unit> sClearSale,
             Fill fi) {
        Cell<Boolean> locked =
                lc.sStart.map(u -> true).merge(
                sClearSale.map(u -> false)).hold(false);
        sStart = lc.sStart.gate(locked.map(l -> !l));
        sEnd   = lc.sEnd.gate(locked);
        fuelFlowing =
                sStart.map(f -> Optional.of(f)).merge(
                sEnd.map(f -> Optional.empty())).hold(Optional.empty());
        fillActive =
             sStart.map(f -> Optional.of(f)).merge(
             sClearSale.map(f -> Optional.empty())).hold(Optional.empty());
        sBeep = sClearSale;
        sSaleComplete = Stream.filterOptional(sEnd.snapshot(
            Cell.lift(
                (oFuel, price_, dollars, liters) ->
                    oFuel.isPresent() ? Optional.of(
                           new Sale(oFuel.get(), price_, dollars, liters))
                                      : Optional.empty(),
                fuelFlowing, fi.price, fi.dollarsDelivered,
                fi.litersDelivered)));
    }
}

