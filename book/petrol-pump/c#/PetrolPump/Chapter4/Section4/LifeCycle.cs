using Sodium;

namespace PetrolPump.Chapter4.Section4
{
    internal class LifeCycle
    {
        private static Stream<Fuel> WhenLifted(Stream<UpDown> sNozzle, Fuel nozzleFuel)
        {
            return sNozzle.Filter(u => u == UpDown.Up).Map(u => nozzleFuel);
        }

        private static Stream<End> WhenSetDown(Stream<UpDown> sNozzle, Fuel nozzleFuel, Cell<IMaybe<Fuel>> fillActive)
        {
            return sNozzle.Snapshot(fillActive, (u, f) => u == UpDown.Down && f.Equals(Maybe.Just(nozzleFuel)) ? Maybe.Just(End.Value) : Maybe.Nothing<End>()).FilterMaybe();
        }

        public LifeCycle(Stream<UpDown> sNozzle1, Stream<UpDown> sNozzle2, Stream<UpDown> sNozzle3)
        {
            Stream<Fuel> sLiftNozzle =
                WhenLifted(sNozzle1, Fuel.One).OrElse(
                    WhenLifted(sNozzle2, Fuel.Two).OrElse(
                        WhenLifted(sNozzle3, Fuel.Three)));
            CellLoop<IMaybe<Fuel>> fillActive = new CellLoop<IMaybe<Fuel>>();
            this.FillActive = fillActive;
            this.SStart = sLiftNozzle.Snapshot(fillActive, (newFuel, fillActiveLocal) => fillActiveLocal.Match(_ => Maybe.Nothing<Fuel>(), () => Maybe.Just(newFuel))).FilterMaybe();
            this.SEnd = WhenSetDown(sNozzle1, Fuel.One, fillActive).OrElse(
                WhenSetDown(sNozzle2, Fuel.Two, fillActive).OrElse(
                    WhenSetDown(sNozzle3, Fuel.Three, fillActive)));
            fillActive.Loop(
                this.SEnd.Map(e => Maybe.Nothing<Fuel>())
                    .OrElse(this.SStart.Map(Maybe.Just))
                    .Hold(Maybe.Nothing<Fuel>()));
        }

        public Cell<IMaybe<Fuel>> FillActive { get; }
        public Stream<Fuel> SStart { get; }
        public Stream<End> SEnd { get; }
    }
}