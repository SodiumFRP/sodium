package chapter4.section9;

import pump.*;
import nz.sodium.*;
import java.util.Optional;

public class Keypad {
    public final Cell<Integer> value;
    public final Stream<Unit> sBeep;

    public Keypad(Stream<Key> sKeypad,
                  Stream<Unit> sClear,
                  Cell<Boolean> active) {
        this(sKeypad.gate(active), sClear);
    }

    public Keypad(Stream<Key> sKeypad, Stream<Unit> sClear) {
        CellLoop<Integer> value = new CellLoop<>();
        this.value = value;
        Stream<Integer> sKeyUpdate = Stream.filterOptional(
            sKeypad.snapshot(value,
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
        value.loop(sKeyUpdate.orElse(sClear.map(u -> 0))
                             .hold(0));
        sBeep = sKeyUpdate.map(k -> Unit.UNIT);
    }
}

