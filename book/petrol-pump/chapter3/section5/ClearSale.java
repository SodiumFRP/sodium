package chapter3.section5;

import pump.*;
import chapter3.section2.LifeCycle;
import chapter3.section2.LifeCycle.End;
import chapter3.section2.LifeCycle.LifeCycleOut;
import chapter3.section3.AccumulatePulses;
import chapter3.section4.ShowDollars;
import chapter3.section4.ShowDollars.FillOut;
import sodium.*;
import java.util.Optional;

public class ClearSale implements Pump
{
    public Outputs create(Inputs inputs)
    {
        EventLoop<Fuel> eStart = new EventLoop<>();

        FillOut fo = ShowDollars.fill(
                          inputs.eClearSale.map(u -> Unit.UNIT),
                          inputs.eFuelPulses, inputs.calibration,
                          inputs.price1, inputs.price2, inputs.price3,
                          eStart);

        NotifyPOSOut np = notifyPointOfSale(
                LifeCycle.lifeCycle(inputs.eNozzle1,
                                    inputs.eNozzle2,
                                    inputs.eNozzle3),
                inputs.eClearSale,
                fo);
        eStart.loop(np.eStart);

        return new Outputs()
            .setDelivery(np.filling.map(
                of ->
                    of.equals(Optional.of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.equals(Optional.of(Fuel.TWO))   ? Delivery.FAST2 :
                    of.equals(Optional.of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF))
            .setSaleCostLCD(fo.dollarsDelivered.map(
                    q -> Formatters.formatSaleCost(q)))
            .setSaleQuantityLCD(fo.litersDelivered.map(
                    q -> Formatters.formatSaleQuantity(q)))
            .setPriceLCD1(ShowDollars.priceLCD(np.fillActive, fo.price,
                    Fuel.ONE, inputs))
            .setPriceLCD2(ShowDollars.priceLCD(np.fillActive, fo.price,
                    Fuel.TWO, inputs))
            .setPriceLCD3(ShowDollars.priceLCD(np.fillActive, fo.price,
                    Fuel.THREE, inputs))
            .setBeep(np.eBeep)
            .setSaleComplete(np.eSaleComplete);
    }

    public static class NotifyPOSOut {
        NotifyPOSOut(Event<Fuel> eStart,
                     Behavior<Optional<Fuel>> fillActive,
                     Behavior<Optional<Fuel>> filling,
                     Event<End> eEnd,
                     Event<Unit> eBeep,
                     Event<Sale> eSaleComplete)
        {
            this.eStart = eStart;
            this.fillActive = fillActive;
            this.filling = filling;
            this.eEnd = eEnd;
            this.eBeep = eBeep;
            this.eSaleComplete = eSaleComplete;
        }
        public final Event<Fuel> eStart;
        public final Behavior<Optional<Fuel>> fillActive;
        public final Behavior<Optional<Fuel>> filling;
        public final Event<End> eEnd;
        public final Event<Unit> eBeep;
        public final Event<Sale> eSaleComplete;
    };

    public static NotifyPOSOut notifyPointOfSale(
             LifeCycleOut lc,
             Event<Unit> eClearSale,
             FillOut fo)
    {
        Behavior<Boolean> locked =
                lc.eStart.map(u -> true).merge(
                eClearSale.map(u -> false)).hold(false);

        Event<Fuel> eStart = lc.eStart.gate(locked.map(l -> !l));
        Event<End> eEnd    = lc.eEnd.gate(locked);

        Behavior<Optional<Fuel>> filling =
                eStart.map(f -> Optional.of(f)).merge(
                eEnd.map(f -> Optional.empty())).hold(Optional.empty());
        Behavior<Optional<Fuel>> fillActive =
             eStart.map(f -> Optional.of(f)).merge(
             eClearSale.map(f -> Optional.empty())).hold(Optional.empty());

        Event<Unit> eBeep = eClearSale;

        Event<Sale> eSaleComplete = Event.filterOptional(eEnd.snapshot(
            Behavior.lift(
                (oFuel, price_, dollars, liters) ->
                    oFuel.isPresent() ? Optional.of(
                           new Sale(oFuel.get(), price_, dollars, liters))
                                      : Optional.empty(),
                filling, fo.price, fo.dollarsDelivered,
                fo.litersDelivered)));

        return new NotifyPOSOut(eStart, fillActive, filling, eEnd, eBeep,
            eSaleComplete);
    }
}

