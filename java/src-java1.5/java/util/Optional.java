package java.util;

public final class Optional<T>
{
    private T t;

    private Optional(T t) {
        this.t = t;
    }

    public static <T> Optional<T> empty() {
        return new Optional<T>(null);
    }

    public boolean equals(Object obj_) {
        if (obj_ instanceof Optional) {
            Optional<T> obj = (Optional<T>)obj_;
            return t != null && obj.t != null ? t.equals(obj.t)
                                              : t == obj.t;
        }
        else
            return false;
    }

    public T get() {
        if (t == null)
            throw new NoSuchElementException();
        else
            return t;
    }

    public int hashCode() {
        return t == null ? 0 : t.hashCode();
    }

    public boolean isPresent() {
        return t != null;
    }

    public static <T> Optional<T> of(T t) {
        if (t == null)
            throw new NullPointerException();
        return new Optional<T>(t);
    }

    public static <T> Optional<T> ofNullable(T t) {
        return new Optional<T>(t);
    }

    public T orElse(T other) {
        return t == null ? other : t;
    }

    public String toString() {
        return t == null ? "" : t.toString();
    }
}
