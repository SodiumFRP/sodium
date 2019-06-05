using System;

namespace Sodium.Frp
{
    internal static class LazyExtensionMethodsInternal
    {
        internal static Lazy<TResult> MapImpl<T, TResult>(this Lazy<T> a, Func<T, TResult> f) => new Lazy<TResult>(() => f(a.Value));

        internal static Lazy<TResult> LiftImpl<T1, T2, TResult>(this Lazy<T1> a, Lazy<T2> b, Func<T1, T2, TResult> f) => new Lazy<TResult>(() => f(a.Value, b.Value));

        internal static Lazy<TResult> LiftImpl<T1, T2, T3, TResult>(
            this Lazy<T1> a,
            Lazy<T2> b,
            Lazy<T3> c,
            Func<T1, T2, T3, TResult> f) =>
            new Lazy<TResult>(() => f(a.Value, b.Value, c.Value));

        internal static Lazy<TResult> LiftImpl<T1, T2, T3, T4, TResult>(
            this Lazy<T1> a,
            Lazy<T2> b,
            Lazy<T3> c,
            Lazy<T4> d,
            Func<T1, T2, T3, T4, TResult> f) =>
            new Lazy<TResult>(() => f(a.Value, b.Value, c.Value, d.Value));
    }
}