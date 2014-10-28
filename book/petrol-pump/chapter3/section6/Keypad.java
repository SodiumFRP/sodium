package chapter3.section6;

import pump.*;
import chapter3.section3.AccumulatePulses;
import chapter3.section4.ShowDollars;
import chapter3.section4.ShowDollars.FillOut;
import sodium.*;
import java.util.Optional;

public class Keypad implements Pump
{
    public Outputs create(Inputs inputs)
    {
        KeypadOut ko = keypad(inputs.eKeypad, new Event<Unit>());
        return new Outputs()
            .setPresetLCD(ko.value.map(v ->
                Formatters.formatSaleCost((double)v)))
            .setBeep(ko.eBeep);
    }

    public static class KeypadOut {
        public KeypadOut(Behavior<Integer> value, Event<Unit> eBeep)
        {
            this.value = value;
            this.eBeep = eBeep;
        }
        public final Behavior<Integer> value;
        public final Event<Unit> eBeep;
    }

    public static KeypadOut keypad(Event<Key> eKeypad, Event<Unit> eClear)
    {
        return lockableKeypad(eKeypad, eClear, new Behavior<>(true));
    }

    public static KeypadOut lockableKeypad(Event<Key> eKeypad,
                                           Event<Unit> eClear,
                                           Behavior<Boolean> active)
    {
        BehaviorLoop<Integer> value = new BehaviorLoop<>();
        Event<Integer> eKeyUpdate = Event.filterOptional(
            eKeypad.gate(active).snapshot(value,
                (key, value_) -> {
                    if (key == Key.CLEAR)
                        return Optional.of(0);
                    else {
                        int x10 = value_ * 10;
                        return x10 >= 1000
                                ? Optional.empty()
                                : Optional.of(
                                    key == Key.ZERO ? x10 :
                                    key == Key.ONE ? x10 + 1 :
                                    key == Key.TWO ? x10 + 2 :
                                    key == Key.THREE ? x10 + 3 : 
                                    key == Key.FOUR ? x10 + 4 : 
                                    key == Key.FIVE ? x10 + 5 : 
                                    key == Key.SIX ? x10 + 6 : 
                                    key == Key.SEVEN ? x10 + 7 : 
                                    key == Key.EIGHT ? x10 + 8 : 
                                                       x10 + 9
                                );
                    }
                }
            )
        );

        value.loop(eKeyUpdate.merge(eClear.map(u -> 0))
                             .hold(0));
        Event<Unit> eBeep = eKeyUpdate.map(k -> Unit.UNIT);
        return new KeypadOut(value, eBeep);
    }
}

