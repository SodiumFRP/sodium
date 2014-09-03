package chapter3.section7;

import pump.*;
import chapter3.section2.LifeCycle;
import chapter3.section2.LifeCycle.End;
import chapter3.section2.LifeCycle.LifeCycleOut;
import chapter3.section3.AccumulatePulses;
import chapter3.section4.ShowDollars;
import chapter3.section4.ShowDollars.FillOut;
import chapter3.section5.ClearSale;
import chapter3.section5.ClearSale.NotifyPOSOut;
import chapter3.section6.Keypad;
import chapter3.section6.Keypad.KeypadOut;
import sodium.*;
import java.util.Optional;

public class PresetAmount implements Pump
{
    public Outputs create(Inputs inputs)
    {
        EventLoop<Fuel> eStart = new EventLoop<>();

        FillOut fo = ShowDollars.fill(
                          inputs.eClearSale.map(u -> Unit.UNIT),
                          inputs.eFuelPulses, inputs.calibration,
                          inputs.price1, inputs.price2, inputs.price3,
                          eStart);

        NotifyPOSOut np = ClearSale.notifyPointOfSale(
                LifeCycle.lifeCycle(inputs.eNozzle1,
                                    inputs.eNozzle2,
                                    inputs.eNozzle3),
                inputs.eClearSale,
                fo);
        eStart.loop(np.eStart);

        BehaviorLoop<Boolean> keypadActive = new BehaviorLoop<>();
        KeypadOut ko = Keypad.lockableKeypad(inputs.eKeypad,
                                             inputs.eClearSale,
                                             keypadActive);

        PresetOut po = preset(ko.value,
                              fo,
                              np.filling,
                              np.fillActive.map(o -> o.isPresent()));
        keypadActive.loop(po.keypadActive);

        return new Outputs()
            .setDelivery(po.delivery)
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
            .setSaleComplete(np.eSaleComplete)
            .setPresetLCD(ko.value.map(v ->
                Formatters.formatSaleCost((double)v)))
            .setBeep(np.eBeep.merge(ko.eBeep));
    }

    public class PresetOut {
        public PresetOut(Behavior<Delivery> delivery,
                         Behavior<Boolean> keypadActive)
        {
            this.delivery = delivery;
            this.keypadActive = keypadActive;
        }
        Behavior<Delivery> delivery;
        Behavior<Boolean> keypadActive;
    }

    public enum Speed { FAST, SLOW, STOPPED };

    public PresetOut preset(Behavior<Integer> presetDollars,
                            FillOut fo,
                            Behavior<Optional<Fuel>> filling,
                            Behavior<Boolean> fillActive)
    {
        Behavior<Speed> speed = Behavior.lift(
            (presetDollars_, price, dollarsDelivered, litersDelivered) -> {
                if (presetDollars_ == 0)
                    return Speed.FAST;
                else {
                    if (dollarsDelivered >= (double)presetDollars_)
                        return Speed.STOPPED;
                    double slowLiters =
                            (double)presetDollars_/price - 0.10;
                    if (litersDelivered >= slowLiters)
                        return Speed.SLOW;
                    else
                        return Speed.FAST;
                }
            },
            presetDollars, fo.price, fo.dollarsDelivered,
                                     fo.litersDelivered);

        Behavior<Delivery> delivery = Behavior.lift(
            (of, speed_) ->
                speed_ == Speed.FAST ? (
                    of.equals(Optional.of(Fuel.ONE))   ? Delivery.FAST1 :
                    of.equals(Optional.of(Fuel.TWO))   ? Delivery.FAST2 :
                    of.equals(Optional.of(Fuel.THREE)) ? Delivery.FAST3 : 
                                                         Delivery.OFF
                ) :
                speed_ == Speed.SLOW ? (
                    of.equals(Optional.of(Fuel.ONE))   ? Delivery.SLOW1 :
                    of.equals(Optional.of(Fuel.TWO))   ? Delivery.SLOW2 :
                    of.equals(Optional.of(Fuel.THREE)) ? Delivery.SLOW3 : 
                                                         Delivery.OFF
                ) :
                Delivery.OFF,
            filling, speed);

        Behavior<Boolean> keypadActive = Behavior.lift(
            (of, speed_) ->
                !of.isPresent() || speed_ == Speed.FAST,
            filling, speed);

        return new PresetOut(delivery, keypadActive);
    }
}

