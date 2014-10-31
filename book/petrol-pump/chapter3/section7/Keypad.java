package chapter3.section7;

import pump.*;
import sodium.*;
import java.util.Optional;

public class Keypad {
    public final Behavior<Integer> value;
    public final Event<Unit> eBeep;

    public Keypad(Event<Key> eKeypad,
                  Event<Unit> eClear,
                  Behavior<Boolean> active) {
        this(eKeypad.gate(active), eClear);
    }

    public Keypad(Event<Key> eKeypad, Event<Unit> eClear) {
        BehaviorLoop<Integer> value = new BehaviorLoop<>();
        this.value = value;
        Event<Integer> eKeyUpdate = Event.filterOptional(
            eKeypad.snapshot(value,
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
        eBeep = eKeyUpdate.map(k -> Unit.UNIT);
    }
}

