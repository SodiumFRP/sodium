public class Mode {
    public enum Type {
        LOCKED,
        OPEN,
        PREPAID
    }

    public Mode(Type mode) {
        this.mode = mode;
    }

    public Mode(Type mode, Double prepayAmount) {
        this.mode = mode;
        this.prepayAmount = prepayAmount;
    }

    public final Type mode;
    public final Double prepayAmount;
}

