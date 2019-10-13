package nz.sodium;

import java.util.Optional;

/**
 * A representation for a value that may not be available until the current
 * transaction is closed.
 */
public class Lazy<A> {
    public Lazy(Lambda0<A> f) { this.f = f; }
    public Lazy(final A a) { this.f = new Lambda0<A>() {
            public A apply() { return a; }
        };
    }
    private Lambda0<A> f;

    /**
     * Get the value if available, throwing an exception if not.
     * In the general case this should only be used in subsequent transactions to
     * when the Lazy was obtained.
     */
    public final A get() {
        return f.apply();
    }

    /**
     * Map the lazy value according to the specified function, so the returned Lazy reflects
     * the value of the function applied to the input Lazy's value.
     * @param f Function to apply to the contained value. It must be <em>referentially transparent</em>.
     */
    public final <B> Lazy<B> map(final Lambda1<A, B> f) {
        return new Lazy<B>(new Lambda0<B>() {
            public B apply() {
                return f.apply(get());
            }
        });
    }

	/**
	 * Lift a binary function into lazy values, so the returned Lazy reflects
     * the value of the function applied to the input Lazys' values.
	 */
	public final <B,C> Lazy<C> lift(final Lazy<B> b, final Lambda2<A,B,C> f)
	{
		return new Lazy<C>(new Lambda0<C>() {
            public C apply() {
                return f.apply(Lazy.this.get(), b.get());
            }
		});
	}

	/**
	 * Lift a ternary function into lazy values, so the returned Lazy reflects
     * the value of the function applied to the input Lazys' values.
	 */
	public final <B,C,D> Lazy<D> lift(final Lazy<B> b, final Lazy<C> c, final Lambda3<A,B,C,D> f)
	{
		return new Lazy<D>(new Lambda0<D>() {
            public D apply() {
                return f.apply(Lazy.this.get(), b.get(), c.get());
            }
		});
	}

	/**
	 * Lift a quaternary function into lazy values, so the returned Lazy reflects
     * the value of the function applied to the input Lazys' values.
	 */
	public final <B,C,D,E> Lazy<E> lift(final Lazy<B> b, final Lazy<C> c, final Lazy<D> d, final Lambda4<A,B,C,D,E> f)
	{
		return new Lazy<E>(new Lambda0<E>() {
            public E apply() {
                return f.apply(Lazy.this.get(), b.get(), c.get(), d.get());
            }
		});
	}

	/**
	 * Lift a binary function into lazy values, so the returned Lazy reflects
     * the value of the function applied to the input Lazys' values.
 	 * @deprecated As of release 1.1.2, replaced by {@link #lift(Lazy,Lambda2)}
	 */
    @Deprecated
	public static final <A,B,C> Lazy<C> lift(final Lambda2<A,B,C> f, final Lazy<A> a, final Lazy<B> b)
	{
		return new Lazy<C>(new Lambda0<C>() {
            public C apply() {
                return f.apply(a.get(), b.get());
            }
		});
	}

	/**
	 * Lift a ternary function into lazy values, so the returned Lazy reflects
     * the value of the function applied to the input Lazys' values.
 	 * @deprecated As of release 1.1.2, replaced by {@link #lift(Lazy,Lazy,Lambda3)}
	 */
    @Deprecated
	public static final <A,B,C,D> Lazy<D> lift(final Lambda3<A,B,C,D> f, final Lazy<A> a, final Lazy<B> b, final Lazy<C> c)
	{
		return new Lazy<D>(new Lambda0<D>() {
            public D apply() {
                return f.apply(a.get(), b.get(), c.get());
            }
		});
	}

	/**
	 * Lift a quaternary function into lazy values, so the returned Lazy reflects
     * the value of the function applied to the input Lazys' values.
 	 * @deprecated As of release 1.1.2, replaced by {@link #lift(Lazy,Lazy,Lazy,Lambda4)}
	 */
	public static final <A,B,C,D,E> Lazy<E> lift(final Lambda4<A,B,C,D,E> f, final Lazy<A> a, final Lazy<B> b, final Lazy<C> c, final Lazy<D> d)
	{
		return new Lazy<E>(new Lambda0<E>() {
            public E apply() {
                return f.apply(a.get(), b.get(), c.get(), d.get());
            }
		});
	}
}

