package pump;

import sodium.*;

public class Outputs {
    private Outputs(
            Behavior<Delivery> delivery,
            Behavior<String> presetLCD,
            Behavior<String> saleCostLCD,
            Behavior<String> saleQuantityLCD,
            Behavior<String> priceLCD1,
            Behavior<String> priceLCD2,
            Behavior<String> priceLCD3,
            Event<Unit> eBeep) {
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
        return new Outputs(delivery, presetLCD, saleCostLCD,
                saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
    }
    public Outputs setPresetLCD(Behavior<String> presetLCD) {
        return new Outputs(delivery, presetLCD, saleCostLCD,
                saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
    }
    public Outputs setSaleCostLCD(Behavior<String> saleCostLCD) {
        return new Outputs(delivery, presetLCD, saleCostLCD,
                saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
    }
    public Outputs setSaleQuantityLCD(Behavior<String> saleQuantityLCD) {
        return new Outputs(delivery, presetLCD, saleCostLCD,
                saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
    }
    public Outputs setPriceLCD1(Behavior<String> priceLCD1) {
        return new Outputs(delivery, presetLCD, saleCostLCD,
                saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
    }
    public Outputs setPriceLCD2(Behavior<String> priceLCD2) {
        return new Outputs(delivery, presetLCD, saleCostLCD,
                saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
    }
    public Outputs setPriceLCD3(Behavior<String> priceLCD3) {
        return new Outputs(delivery, presetLCD, saleCostLCD,
                saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
    }
    public Outputs setBeep(Event<Unit> eBeep) {
        return new Outputs(delivery, presetLCD, saleCostLCD,
                saleQuantityLCD, priceLCD1, priceLCD2, priceLCD3, eBeep);
    }
}

