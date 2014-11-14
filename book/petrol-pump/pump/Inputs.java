package pump;

import sodium.*;

public class Inputs {
    public Inputs(
            Event<UpDown> eNozzle1,
            Event<UpDown> eNozzle2,
            Event<UpDown> eNozzle3,
            Event<Key> eKeypad,
            Event<Integer> eFuelPulses,
            Behavior<Double> calibration,
            Behavior<Double> price1,
            Behavior<Double> price2,
            Behavior<Double> price3,
            Event<Unit> eClearSale) {
        this.eNozzle1 = eNozzle1;
        this.eNozzle2 = eNozzle2;
        this.eNozzle3 = eNozzle3;
        this.eKeypad = eKeypad;
        this.eFuelPulses = eFuelPulses;
        this.calibration = calibration;
        this.price1 = price1;
        this.price2 = price2;
        this.price3 = price3;
        this.eClearSale = eClearSale;
    }

    public final Event<UpDown> eNozzle1;
    public final Event<UpDown> eNozzle2;
    public final Event<UpDown> eNozzle3;
    public final Event<Key> eKeypad;
    public final Event<Integer> eFuelPulses;
    public final Behavior<Double> calibration;
    public final Behavior<Double> price1;
    public final Behavior<Double> price2;
    public final Behavior<Double> price3;
    public final Event<Unit> eClearSale;
}
