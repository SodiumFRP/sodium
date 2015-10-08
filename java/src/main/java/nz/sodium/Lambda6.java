package nz.sodium;

/**
 * An interface for 6-argument lambda functions.
 */
public interface Lambda6<A,B,C,D,E,F,G> {
    G apply(A a, B b, C c, D d, E e, F f);
}

