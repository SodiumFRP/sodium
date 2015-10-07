package nz.sodium;

/**
 * An interface for 4-argument lambda functions.
 */
public interface Lambda4<A,B,C,D,E> {
    E apply(A a, B b, C c, D d);
}

