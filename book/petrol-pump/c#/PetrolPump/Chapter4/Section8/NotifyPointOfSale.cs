using PetrolPump.Chapter4.Section4;
using PetrolPump.Chapter4.Section7;
using Sodium;

namespace PetrolPump.Chapter4.Section8
{
    internal class NotifyPointOfSale
    {
        public NotifyPointOfSale(
            LifeCycle lc,
            Stream<Unit> sClearSale,
            Fill fi)
        {
            DiscreteCell<bool> locked = lc.SStart.Map(u => true).OrElse(sClearSale.Map(u => false)).Hold(false);
            this.SStart = lc.SStart.Gate(locked.Map(l => !l));
            this.SEnd = lc.SEnd.Gate(locked);
            this.FuelFlowing =
                this.SStart.Map(Maybe.Just).OrElse(
                    this.SEnd.Map(f => Maybe.Nothing<Fuel>())).Hold(Maybe.Nothing<Fuel>());
            this.FillActive =
                this.SStart.Map(Maybe.Just).OrElse(
                    sClearSale.Map(f => Maybe.Nothing<Fuel>())).Hold(Maybe.Nothing<Fuel>());
            this.SBeep = sClearSale;
            this.SSaleComplete = this.SEnd.Snapshot(
                this.FuelFlowing.Lift(
                    fi.Price, fi.DollarsDelivered,
                    fi.LitersDelivered,
                    (oFuel, price, dollars, liters) =>
                        oFuel.Match(v => Maybe.Just(
                            new Sale(v, price, dollars, liters)),
                            Maybe.Nothing<Sale>))).FilterMaybe();
        }

        public Stream<Fuel> SStart { get; }
        public DiscreteCell<IMaybe<Fuel>> FillActive { get; }
        public DiscreteCell<IMaybe<Fuel>> FuelFlowing { get; }
        public Stream<End> SEnd { get; }
        public Stream<Unit> SBeep { get; }
        public Stream<Sale> SSaleComplete { get; }
    }
}