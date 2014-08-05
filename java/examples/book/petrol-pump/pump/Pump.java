package pump;

import sodium.*;

public interface Pump
{
    public static class Inputs {
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
            Behavior<Integer> quantityPlaces
        )
        {
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

    public static class Outputs {
        private Outputs(
            Behavior<Delivery> delivery,
            Behavior<String> presetLCD,
            Behavior<String> saleCostLCD,
            Behavior<String> saleQuantityLCD,
            Behavior<String> priceLCD1,
            Behavior<String> priceLCD2,
            Behavior<String> priceLCD3,
            Event<Unit> eBeep
        ) {
            this.delivery = delivery;
            this.presetLCD = presetLCD;
            this.saleCostLCD = saleCostLCD;
            this.saleQuantityLCD = saleQuantityLCD;
            this.priceLCD1 = priceLCD1;
            this.priceLCD2 = priceLCD2;
            this.priceLCD3 = priceLCD3;
            this.eBeep = eBeep;
        }

        public Outputs() {
            this.delivery = new Behavior<Delivery>(Delivery.OFF);
            this.presetLCD = new Behavior<String>("");
            this.saleCostLCD = new Behavior<String>("");
            this.saleQuantityLCD = new Behavior<String>("");
            this.priceLCD1 = new Behavior<String>("");
            this.priceLCD2 = new Behavior<String>("");
            this.priceLCD3 = new Behavior<String>("");
            this.eBeep = new Event<Unit>();
        }

        public final Behavior<Delivery> delivery;
        public final Behavior<String> presetLCD;
        public final Behavior<String> saleCostLCD;
        public final Behavior<String> saleQuantityLCD;
        public final Behavior<String> priceLCD1;
        public final Behavior<String> priceLCD2;
        public final Behavior<String> priceLCD3;
        public final Event<Unit> eBeep;

        public Outputs setDelivery(Behavior<Delivery> delivery) {
            return new Outputs(delivery, presetLCD, saleCostLCD, saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
        }
        public Outputs setPresetLCD(Behavior<String> presetLCD) {
            return new Outputs(delivery, presetLCD, saleCostLCD, saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
        }
        public Outputs setSaleCostLCD(Behavior<String> saleCostLCD) {
            return new Outputs(delivery, presetLCD, saleCostLCD, saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
        }
        public Outputs setSaleQuantityLCD(Behavior<String> saleQuantityLCD) {
            return new Outputs(delivery, presetLCD, saleCostLCD, saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
        }
        public Outputs setPriceLCD1(Behavior<String> priceLCD1) {
            return new Outputs(delivery, presetLCD, saleCostLCD, saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
        }
        public Outputs setPriceLCD2(Behavior<String> priceLCD2) {
            return new Outputs(delivery, presetLCD, saleCostLCD, saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
        }
        public Outputs setPriceLCD3(Behavior<String> priceLCD3) {
            return new Outputs(delivery, presetLCD, saleCostLCD, saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
        }
        public Outputs setBeep(Event<Unit> eBeep) {
            return new Outputs(delivery, presetLCD, saleCostLCD, saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
        }
    }

    public Outputs create(Inputs inputs);
}

