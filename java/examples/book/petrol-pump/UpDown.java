public enum UpDown {
    UP, DOWN;
    public final UpDown invert() {
        return this == UP ? DOWN : UP;
    }
}

