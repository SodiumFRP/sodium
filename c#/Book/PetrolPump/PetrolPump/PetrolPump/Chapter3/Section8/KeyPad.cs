using System;

using PetrolPump.Pump;

using Sodium;

namespace PetrolPump.Chapter3.Section8
{
  public class Keypad
  {
    public readonly Behavior<int> Value;
    public readonly Event<Unit> EBeep;

    public Keypad(
      Event<Key> eKeypad,
      Event<Unit> eClear,
      Behavior<Boolean> active)
      : this(eKeypad.Gate(active), eClear)
    {}

    public Keypad(Event<Key> eKeypad, Event<Unit> eClear)
    {
      BehaviorLoop<int> value = new BehaviorLoop<int>();
      this.Value = value;
      Event<int> eKeyUpdate = Event<int>.FilterOptional(
        eKeypad.Snapshot(
          value,
          (key, value_) =>
          {
            if (key == Key.CLEAR)
              return Optional<int>.Of(0);
            else
            {
              int x10 = value_ * 10;
              return x10 >= 1000
                ? Optional<int>.Empty()
                : Optional<int>.Of(
                  key == Key.ZERO
                    ? x10
                    : key == Key.ONE
                      ? x10 + 1
                      : key == Key.TWO
                        ? x10 + 2
                        : key == Key.THREE
                          ? x10 + 3
                          : key == Key.FOUR
                            ? x10 + 4
                            : key == Key.FIVE
                              ? x10 + 5
                              : key == Key.SIX
                                ? x10 + 6
                                : key == Key.SEVEN
                                  ? x10 + 7
                                  : key == Key.EIGHT
                                    ? x10 + 8
                                    : x10 + 9
                  );
            }
          })
        );
      value.Loop(
        eKeyUpdate.Merge(eClear.Map(u => 0))
          .Hold(0));
      EBeep = eKeyUpdate.Map(k => Unit.UNIT);
    }
  }

}
