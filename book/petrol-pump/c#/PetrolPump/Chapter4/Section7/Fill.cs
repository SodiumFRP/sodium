using PetrolPump.Chapter4.Section6;
using Sodium;

namespace PetrolPump.Chapter4.Section7
{
    internal class Fill
    {
        public Fill(
            Stream<Unit> sClearAccumulator, Stream<int> sFuelPulses,
            DiscreteCell<double> calibration, DiscreteCell<double> price1,
            DiscreteCell<double> price2, DiscreteCell<double> price3,
            Stream<Fuel> sStart)
        {
            this.Price = CapturePrice(sStart, price1, price2, price3);
            this.LitersDelivered = AccumulatePulsesPump.Accumulate(
                sClearAccumulator, sFuelPulses, calibration);
            this.DollarsDelivered = this.LitersDelivered.Lift(this.Price,
                (liters, price) => liters * price);
        }

        public static DiscreteCell<double> CapturePrice(
            Stream<Fuel> sStart,
            DiscreteCell<double> price1, DiscreteCell<double> price2,
            DiscreteCell<double> price3)
        {
            Stream<double> sPrice1 = sStart.Snapshot(price1,
                (f, p) => f == Fuel.One ? Maybe.Some(p) : Maybe.None).FilterMaybe();
            Stream<double> sPrice2 = sStart.Snapshot(price2,
                (f, p) => f == Fuel.Two ? Maybe.Some(p) : Maybe.None).FilterMaybe();
            Stream<double> sPrice3 = sStart.Snapshot(price3,
                (f, p) => f == Fuel.Three ? Maybe.Some(p) : Maybe.None).FilterMaybe();

            return sPrice1.OrElse(sPrice2.OrElse(sPrice3)).Hold(0.0);
        }

        public DiscreteCell<double> Price { get; }
        public DiscreteCell<double> DollarsDelivered { get; }
        public DiscreteCell<double> LitersDelivered { get; }
    }
}