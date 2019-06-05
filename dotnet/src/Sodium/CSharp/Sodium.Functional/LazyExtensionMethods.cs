using System;

namespace Sodium.Functional
{
    public static class LazyExtensionMethods
    {
        /// <summary>
        ///     Map the lazy input value according to the specified function so the returned Lazy reflects the value of the
        ///     function applied to the lazy input's value.
        /// </summary>
        /// <typeparam name="T">The type of the lazy input value.</typeparam>
        /// <typeparam name="TResult">The type of the lazy return value.</typeparam>
        /// <param name="a">The lazy input.</param>
        /// <param name="f">The function to transform the lazy input value.</param>
        /// <returns>
        ///     A lazy value which will produce the value of the lazy input value <paramref name="a" /> transformed by the
        ///     function <paramref name="f" />.
        /// </returns>
        public static Lazy<TResult> Map<T, TResult>(this Lazy<T> a, Func<T, TResult> f)
        {
            return new Lazy<TResult>(() => f(a.Value));
        }

        //      /**
        //* Lift a binary function into lazy values, so the returned Lazy reflects
        //   * the value of the function applied to the input Lazys' values.
        //*/

        /// <summary>
        ///     Lift a binary function into lazy input values so the lazy return value reflects the value of the function applied
        ///     to the lazy input values.
        /// </summary>
        /// <typeparam name="T1">The type of the first lazy input value.</typeparam>
        /// <typeparam name="T2">The type of the second lazy input value.</typeparam>
        /// <typeparam name="TResult">The type of the lazy return value.</typeparam>
        /// <param name="a">The first lazy input.</param>
        /// <param name="b">The second lazy input.</param>
        /// <param name="f">The function to transform the lazy input value.</param>
        /// <returns>
        ///     A lazy value which will produce the value of the lazy input values <paramref name="a" /> and
        ///     <paramref name="b" /> transformed by the function <paramref name="f" />.
        /// </returns>
        public static Lazy<TResult> Lift<T1, T2, TResult>(this Lazy<T1> a, Lazy<T2> b, Func<T1, T2, TResult> f)
        {
            return new Lazy<TResult>(() => f(a.Value, b.Value));
        }

        /// <summary>
        ///     Lift a binary function into lazy input values so the lazy return value reflects the value of the function applied
        ///     to the lazy input values.
        /// </summary>
        /// <typeparam name="T1">The type of the first lazy input value.</typeparam>
        /// <typeparam name="T2">The type of the second lazy input value.</typeparam>
        /// <typeparam name="T3">The type of the third lazy input value.</typeparam>
        /// <typeparam name="TResult">The type of the lazy return value.</typeparam>
        /// <param name="a">The first lazy input.</param>
        /// <param name="b">The second lazy input.</param>
        /// <param name="c">The third lazy input.</param>
        /// <param name="f">The function to transform the lazy input value.</param>
        /// <returns>
        ///     A lazy value which will produce the value of the lazy input values <paramref name="a" />,
        ///     <paramref name="b" />, and <paramref name="c" /> transformed by the function <paramref name="f" />.
        /// </returns>
        public static Lazy<TResult> Lift<T1, T2, T3, TResult>(
            this Lazy<T1> a,
            Lazy<T2> b,
            Lazy<T3> c,
            Func<T1, T2, T3, TResult> f)
        {
            return new Lazy<TResult>(() => f(a.Value, b.Value, c.Value));
        }

        /// <summary>
        ///     Lift a binary function into lazy input values so the lazy return value reflects the value of the function applied
        ///     to the lazy input values.
        /// </summary>
        /// <typeparam name="T1">The type of the first lazy input value.</typeparam>
        /// <typeparam name="T2">The type of the second lazy input value.</typeparam>
        /// <typeparam name="T3">The type of the third lazy input value.</typeparam>
        /// <typeparam name="T4">The type of the fourth lazy input value.</typeparam>
        /// <typeparam name="TResult">The type of the lazy return value.</typeparam>
        /// <param name="a">The first lazy input.</param>
        /// <param name="b">The second lazy input.</param>
        /// <param name="c">The third lazy input.</param>
        /// <param name="d">The fourth lazy input.</param>
        /// <param name="f">The function to transform the lazy input value.</param>
        /// <returns>
        ///     A lazy value which will produce the value of the lazy input values <paramref name="a" />,
        ///     <paramref name="b" />, <paramref name="c" />, and <paramref name="d" /> transformed by the function
        ///     <paramref name="f" />.
        /// </returns>
        public static Lazy<TResult> Lift<T1, T2, T3, T4, TResult>(
            this Lazy<T1> a,
            Lazy<T2> b,
            Lazy<T3> c,
            Lazy<T4> d,
            Func<T1, T2, T3, T4, TResult> f)
        {
            return new Lazy<TResult>(() => f(a.Value, b.Value, c.Value, d.Value));
        }
    }
}