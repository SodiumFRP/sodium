package pump;

import nz.sodium.*;

public class Inputs {
    public Inputs(
            Stream<UpDown> sNozzle1,
            Stream<UpDown> sNozzle2,
            Stream<UpDown> sNozzle3,
            Stream<Key> sKeypad,
            Stream<Integer> sFuelPulses,
            Cell<Double> calibration,
            Cell<Double> price1,
            Cell<Double> price2,
            Cell<Double> price3,
            Stream<Unit> sClearSale) {
        this.sNozzle1 = sNozzle1;
        this.sNozzle2 = sNozzle2;
        this.sNozzle3 = sNozzle3;
        this.sKeypad = sKeypad;
        this.sFuelPulses = sFuelPulses;
        this.calibration = calibration;
        this.price1 = price1;
        this.price2 = price2;
        this.price3 = price3;
        this.sClearSale = sClearSale;
    }

    public final Stream<UpDown> sNozzle1;
    public final Stream<UpDown> sNozzle2;
    public final Stream<UpDown> sNozzle3;
    public final Stream<Key> sKeypad;
    public final Stream<Integer> sFuelPulses;
    public final Cell<Double> calibration;
    public final Cell<Double> price1;
    public final Cell<Double> price2;
    public final Cell<Double> price3;
    public final Stream<Unit> sClearSale;
}
