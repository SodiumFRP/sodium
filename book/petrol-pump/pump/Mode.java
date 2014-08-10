package pump;

public class Mode {
    public enum Type {
        LOCKED,
        OPEN,
        PREPAID
    }

    public static Mode locked() {
        return new Mode(Type.LOCKED, 0.0);
    }
    public static Mode open() {
        return new Mode(Type.OPEN, 0.0);
    }
    public static Mode prepaid(double amount) {
        return new Mode(Type.PREPAID, amount);
    }

    private Mode(Type mode, Double prepayAmount) {
        this.mode = mode;
        this.prepayAmount = prepayAmount;
    }

    public final Type mode;
    public final double prepayAmount;
}

