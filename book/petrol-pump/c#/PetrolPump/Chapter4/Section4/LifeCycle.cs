using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump.Chapter4.Section4
{
    internal class LifeCycle
    {
        private static Stream<Fuel> WhenLifted(Stream<UpDown> sNozzle, Fuel nozzleFuel)
        {
            return sNozzle.Filter(u => u == UpDown.Up).Map(u => nozzleFuel);
        }

        private static Stream<End> WhenSetDown(Stream<UpDown> sNozzle, Fuel nozzleFuel, Cell<Maybe<Fuel>> fillActive) =>
            sNozzle.Snapshot(fillActive, (u, f) => u == UpDown.Down && f.Equals(Maybe.Some(nozzleFuel)) ? Maybe.Some(End.Value) : Maybe.None).FilterMaybe();

        public LifeCycle(Stream<UpDown> sNozzle1, Stream<UpDown> sNozzle2, Stream<UpDown> sNozzle3)
        {
            Stream<Fuel> sLiftNozzle =
                WhenLifted(sNozzle1, Fuel.One).OrElse(
                    WhenLifted(sNozzle2, Fuel.Two).OrElse(
                        WhenLifted(sNozzle3, Fuel.Three)));
            CellLoop<Maybe<Fuel>> fillActive = new CellLoop<Maybe<Fuel>>();
            this.FillActive = fillActive;
            this.SStart = sLiftNozzle.Snapshot(fillActive, (newFuel, fillActiveLocal) => fillActiveLocal.Match(_ => Maybe.None, () => Maybe.Some(newFuel))).FilterMaybe();
            this.SEnd = WhenSetDown(sNozzle1, Fuel.One, fillActive).OrElse(
                WhenSetDown(sNozzle2, Fuel.Two, fillActive).OrElse(
                    WhenSetDown(sNozzle3, Fuel.Three, fillActive)));
            fillActive.Loop(
                this.SEnd.Map(e => Maybe<Fuel>.None)
                    .OrElse(this.SStart.Map(Maybe.Some))
                    .Hold(Maybe.None));
        }

        public Cell<Maybe<Fuel>> FillActive { get; }
        public Stream<Fuel> SStart { get; }
        public Stream<End> SEnd { get; }
    }
}