package chapter3.section5;

import pump.*;
import chapter3.section2.LifeCycle;
import chapter3.section2.LifeCycle.End;
import chapter3.section4.Fill;
import sodium.*;
import java.util.Optional;

public class NotifyPointOfSale {
    public final Event<Fuel> eStart;
    public final Behavior<Optional<Fuel>> fillActive;
    public final Behavior<Optional<Fuel>> fuelFlowing;
    public final Event<End> eEnd;
    public final Event<Unit> eBeep;
    public final Event<Sale> eSaleComplete;

    public NotifyPointOfSale(
             LifeCycle lc,
             Event<Unit> eClearSale,
             Fill fi) {
        Behavior<Boolean> locked =
                lc.eStart.map(u -> true).merge(
                eClearSale.map(u -> false)).hold(false);
        eStart = lc.eStart.gate(locked.map(l -> !l));
        eEnd    = lc.eEnd.gate(locked);
        fuelFlowing =
                eStart.map(f -> Optional.of(f)).merge(
                eEnd.map(f -> Optional.empty())).hold(Optional.empty());
        fillActive =
             eStart.map(f -> Optional.of(f)).merge(
             eClearSale.map(f -> Optional.empty())).hold(Optional.empty());
        eBeep = eClearSale;
        eSaleComplete = Event.filterOptional(eEnd.snapshot(
            Behavior.lift(
                (oFuel, price_, dollars, liters) ->
                    oFuel.isPresent() ? Optional.of(
                           new Sale(oFuel.get(), price_, dollars, liters))
                                      : Optional.empty(),
                fuelFlowing, fi.price, fi.dollarsDelivered,
                fi.litersDelivered)));
    }
}

