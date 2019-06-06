using Sodium.Frp;
using Sodium.Functional;

namespace PetrolPump
{
    public class Inputs
    {
        public Inputs(
            Stream<UpDown> sNozzle1,
            Stream<UpDown> sNozzle2,
            Stream<UpDown> sNozzle3,
            Stream<Key> sKeypad,
            Stream<int> sFuelPulses,
            Cell<double> calibration,
            Cell<double> price1,
            Cell<double> price2,
            Cell<double> price3,
            Stream<Unit> sClearSale)
        {
            this.SNozzle1 = sNozzle1;
            this.SNozzle2 = sNozzle2;
            this.SNozzle3 = sNozzle3;
            this.SKeypad = sKeypad;
            this.SFuelPulses = sFuelPulses;
            this.Calibration = calibration;
            this.Price1 = price1;
            this.Price2 = price2;
            this.Price3 = price3;
            this.SClearSale = sClearSale;
        }

        public Stream<UpDown> SNozzle1 { get; }
        public Stream<UpDown> SNozzle2 { get; }
        public Stream<UpDown> SNozzle3 { get; }
        public Stream<Key> SKeypad { get; }
        public Stream<int> SFuelPulses { get; }
        public Cell<double> Calibration { get; }
        public Cell<double> Price1 { get; }
        public Cell<double> Price2 { get; }
        public Cell<double> Price3 { get; }
        public Stream<Unit> SClearSale { get; }
    }
}