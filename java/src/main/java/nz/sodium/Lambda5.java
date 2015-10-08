package nz.sodium;

/**
 * An interface for 5-argument lambda functions.
 */
public interface Lambda5<A,B,C,D,E,F> {
    F apply(A a, B b, C c, D d, E e);
}

