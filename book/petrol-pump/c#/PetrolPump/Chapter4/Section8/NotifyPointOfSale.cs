using PetrolPump.Chapter4.Section4;
using PetrolPump.Chapter4.Section7;
using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump.Chapter4.Section8
{
    internal class NotifyPointOfSale
    {
        public NotifyPointOfSale(
            LifeCycle lc,
            Stream<Unit> sClearSale,
            Fill fi)
        {
            Cell<bool> locked = lc.SStart.Map(u => true).OrElse(sClearSale.Map(u => false)).Hold(false);
            this.SStart = lc.SStart.Gate(locked.Map(l => !l));
            this.SEnd = lc.SEnd.Gate(locked);
            this.FuelFlowing =
                this.SStart.Map(Maybe.Some).OrElse(
                    this.SEnd.Map(f => Maybe<Fuel>.None)).Hold(Maybe.None);
            this.FillActive =
                this.SStart.Map(Maybe.Some).OrElse(
                    sClearSale.Map(f => Maybe<Fuel>.None)).Hold(Maybe.None);
            this.SBeep = sClearSale;
            this.SSaleComplete = this.SEnd.Snapshot(
                this.FuelFlowing.Lift(
                    fi.Price, fi.DollarsDelivered,
                    fi.LitersDelivered,
                    (oFuel, price, dollars, liters) =>
                        oFuel.Match(
                            v => Maybe.Some(new Sale(v, price, dollars, liters)),
                            () => Maybe.None))).FilterMaybe();
        }

        public Stream<Fuel> SStart { get; }
        public Cell<Maybe<Fuel>> FillActive { get; }
        public Cell<Maybe<Fuel>> FuelFlowing { get; }
        public Stream<End> SEnd { get; }
        public Stream<Unit> SBeep { get; }
        public Stream<Sale> SSaleComplete { get; }
    }
}