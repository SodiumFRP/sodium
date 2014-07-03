import sodium.*;

public interface Pump
{
    public static class Outputs {
        public Outputs(
            Behavior<Delivery> delivery,
            Behavior<String> presetLCD,
            Behavior<String> saleCostLCD,
            Behavior<String> saleQuantityLCD,
            Behavior<String> priceLCD1,
            Behavior<String> priceLCD2,
            Behavior<String> priceLCD3
        ) {
            this.delivery = delivery;
            this.presetLCD = presetLCD;
            this.saleCostLCD = saleCostLCD;
            this.saleQuantityLCD = saleQuantityLCD;
            this.priceLCD1 = priceLCD1;
            this.priceLCD2 = priceLCD2;
            this.priceLCD3 = priceLCD3;
        }

        public final Behavior<Delivery> delivery;
        public final Behavior<String> presetLCD;
        public final Behavior<String> saleCostLCD;
        public final Behavior<String> saleQuantityLCD;
        public final Behavior<String> priceLCD1;
        public final Behavior<String> priceLCD2;
        public final Behavior<String> priceLCD3;
    }

    public Outputs create(
        Behavior<UpDown> nozzle1,
        Behavior<UpDown> nozzle2,
        Behavior<UpDown> nozzle3,
        Event<Key> keypad,
        Event<Integer> fuelPulses,
        Behavior<Double> calibration,
        Behavior<Double> price1,
        Behavior<Double> price2,
        Behavior<Double> price3,
        Behavior<Mode> mode,
        Event<Unit> clearSale,
        Behavior<Double> clock,
        Behavior<Integer> costPlaces,
        Behavior<Integer> quantityPlaces
    );
}

