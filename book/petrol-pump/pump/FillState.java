package pump;

public class FillState {
    public enum Type {
        IDLE,
        FILLING,
        SALE_COMPLETE
    }

    public FillState(Type mode) {
        this.mode = mode;
        this.sale = null;
    }

    public FillState(Type mode, Sale sale) {
        this.mode = mode;
        this.sale = sale;
    }

    public final Type mode;
    public final Sale sale;
}

