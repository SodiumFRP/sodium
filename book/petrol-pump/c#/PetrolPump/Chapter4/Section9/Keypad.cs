using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump.Chapter4.Section9
{
    internal class Keypad
    {
        public Keypad(Stream<Key> sKeypad,
            Stream<Unit> sClear,
            Cell<bool> active)
            : this(sKeypad.Gate(active), sClear)
        {
        }

        public Keypad(Stream<Key> sKeypad, Stream<Unit> sClear)
        {
            CellLoop<int> value = new CellLoop<int>();
            this.Value = value;
            Stream<int> sKeyUpdate = sKeypad.Snapshot(value, (key, valueLocal) =>
            {
                if (key == Key.Clear)
                {
                    return Maybe.Some(0);
                }
                int x10 = valueLocal * 10;
                return x10 >= 1000
                    ? Maybe.None
                    : Maybe.Some(
                        key == Key.Zero ? x10 :
                            key == Key.One ? x10 + 1 :
                                key == Key.Two ? x10 + 2 :
                                    key == Key.Three ? x10 + 3 :
                                        key == Key.Four ? x10 + 4 :
                                            key == Key.Five ? x10 + 5 :
                                                key == Key.Six ? x10 + 6 :
                                                    key == Key.Seven ? x10 + 7 :
                                                        key == Key.Eight ? x10 + 8 :
                                                            x10 + 9);
            }).FilterMaybe();
            value.Loop(sKeyUpdate.OrElse(sClear.Map(u => 0)).Hold(0));
            this.SBeep = sKeyUpdate.Map(_ => Unit.Value);
        }

        public Cell<int> Value { get; }
        public Stream<Unit> SBeep { get; }
    }
}