package chapter3.section9;

import pump.*;
import chapter3.section5.Fill;
import sodium.*;
import java.util.Optional;

public class Preset {
    public final Behavior<Delivery> delivery;
    public final Behavior<Boolean> keypadActive;

    public enum Speed { FAST, SLOW, STOPPED };

    public Preset(Behavior<Integer> presetDollars,
                  Fill fi,
                  Behavior<Optional<Fuel>> fuelFlowing,
                  Behavior<Boolean> fillActive) {
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
            presetDollars, fi.price, fi.dollarsDelivered,
                                     fi.litersDelivered);
        delivery = Behavior.lift(
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
        keypadActive = Behavior.lift(
            (of, speed_) ->
                !of.isPresent() || speed_ == Speed.FAST,
            fuelFlowing, speed);
    }
}
