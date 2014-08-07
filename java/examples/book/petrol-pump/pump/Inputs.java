package pump;

import sodium.*;

public class Inputs {
    public Inputs(
            Behavior<UpDown> nozzle1,
            Behavior<UpDown> nozzle2,
            Behavior<UpDown> nozzle3,
            Event<Key> eKeypad,
            Event<Integer> eFuelPulses,
            Behavior<Double> calibration,
            Behavior<Double> price1,
            Behavior<Double> price2,
            Behavior<Double> price3,
            Behavior<Mode> mode,
            Event<Unit> eClearSale,
            Behavior<Double> clock,
            Behavior<Integer> costPlaces,
            Behavior<Integer> quantityPlaces) {
        this.nozzle1 = nozzle1;
        this.nozzle2 = nozzle2;
        this.nozzle3 = nozzle3;
        this.eKeypad = eKeypad;
        this.eFuelPulses = eFuelPulses;
        this.calibration = calibration;
        this.price1 = price1;
        this.price2 = price2;
        this.price3 = price3;
        this.mode = mode;
        this.eClearSale = eClearSale;
        this.clock = clock;
        this.costPlaces = costPlaces;
        this.quantityPlaces = quantityPlaces;
    }

    public final Behavior<UpDown> nozzle1;
    public final Behavior<UpDown> nozzle2;
    public final Behavior<UpDown> nozzle3;
    public final Event<Key> eKeypad;
    public final Event<Integer> eFuelPulses;
    public final Behavior<Double> calibration;
    public final Behavior<Double> price1;
    public final Behavior<Double> price2;
    public final Behavior<Double> price3;
    public final Behavior<Mode> mode;
    public final Event<Unit> eClearSale;
    public final Behavior<Double> clock;
    public final Behavior<Integer> costPlaces;
    public final Behavior<Integer> quantityPlaces;
}
