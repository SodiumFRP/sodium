using PetrolPump.Chapter4.Section6;
using Sodium;

namespace PetrolPump.Chapter4.Section7
{
    internal class Fill
    {
        public Fill(
            Stream<Unit> sClearAccumulator, Stream<int> sFuelPulses,
            Cell<double> calibration, Cell<double> price1,
            Cell<double> price2, Cell<double> price3,
            Stream<Fuel> sStart)
        {
            this.Price = CapturePrice(sStart, price1, price2, price3);
            this.LitersDelivered = AccumulatePulsesPump.Accumulate(
                sClearAccumulator, sFuelPulses, calibration);
            this.DollarsDelivered = this.LitersDelivered.Lift(this.Price,
                (liters, price) => liters * price);
        }

        public static Cell<double> CapturePrice(
            Stream<Fuel> sStart,
            Cell<double> price1, Cell<double> price2,
            Cell<double> price3)
        {
            Stream<double> sPrice1 = sStart.Snapshot(price1,
                (f, p) => f == Fuel.One ? Maybe.Just(p) : Maybe.Nothing<double>()).FilterMaybe();
            Stream<double> sPrice2 = sStart.Snapshot(price2,
                (f, p) => f == Fuel.Two ? Maybe.Just(p) : Maybe.Nothing<double>()).FilterMaybe();
            Stream<double> sPrice3 = sStart.Snapshot(price3,
                (f, p) => f == Fuel.Three ? Maybe.Just(p) : Maybe.Nothing<double>()).FilterMaybe();

            return sPrice1.OrElse(sPrice2.OrElse(sPrice3)).Hold(0.0);
        }

        public Cell<double> Price { get; }
        public Cell<double> DollarsDelivered { get; }
        public Cell<double> LitersDelivered { get; }
    }
}