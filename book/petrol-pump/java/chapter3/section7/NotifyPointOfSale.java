package chapter3.section7;

import pump.*;
import chapter3.section3.LifeCycle;
import chapter3.section3.LifeCycle.End;
import chapter3.section6.Fill;
import sodium.*;
import java.util.Optional;

public class NotifyPointOfSale {
    public final Stream<Fuel> sStart;
    public final Cell<Optional<Fuel>> fillActive;
    public final Cell<Optional<Fuel>> fuelFlowing;
    public final Stream<End> eEnd;
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
        eEnd    = lc.eEnd.gate(locked);
        fuelFlowing =
                sStart.map(f -> Optional.of(f)).merge(
                eEnd.map(f -> Optional.empty())).hold(Optional.empty());
        fillActive =
             sStart.map(f -> Optional.of(f)).merge(
             sClearSale.map(f -> Optional.empty())).hold(Optional.empty());
        sBeep = sClearSale;
        sSaleComplete = Stream.filterOptional(eEnd.snapshot(
            Cell.lift(
                (oFuel, price_, dollars, liters) ->
                    oFuel.isPresent() ? Optional.of(
                           new Sale(oFuel.get(), price_, dollars, liters))
                                      : Optional.empty(),
                fuelFlowing, fi.price, fi.dollarsDelivered,
                fi.litersDelivered)));
    }
}

