package sodium;

public class Unit {
    public static final Unit unit = new Unit();
    Unit() {}

    @Override
    public boolean equals(Object other_) {
        return other_ instanceof Unit;
    }
};
