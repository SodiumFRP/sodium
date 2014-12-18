package chapter3.section10;

import pump.*;
import chapter3.section6.Fill;
import sodium.*;
import java.util.Optional;

public class Preset {
    public final Cell<Delivery> delivery;
    public final Cell<Boolean> keypadActive;

    public enum Speed { FAST, SLOW, STOPPED };

    public Preset(Cell<Integer> presetDollars,
                  Fill fi,
                  Cell<Optional<Fuel>> fuelFlowing,
                  Cell<Boolean> fillActive) {
        Cell<Speed> speed = Cell.lift(
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
            presetDollars, fi.price, fi.dollarsDelivered,
                                     fi.litersDelivered);
        delivery = Cell.lift(
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
            fuelFlowing, speed);
        keypadActive = Cell.lift(
            (of, speed_) ->
                !of.isPresent() || speed_ == Speed.FAST,
            fuelFlowing, speed);
    }
}
