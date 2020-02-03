using System;

namespace Sodium.Functional
{
    /// <summary>
    ///     Provides methods implementing a monad for <see cref="Maybe{T}" />.
    /// </summary>
    public static class MaybeMonad
    {
        #region Public Methods and Operators

        /// <summary>
        ///     The bind method for the <see cref="Maybe{T}" /> monad, which transforms an <see cref="Maybe{T}" /> into an
        ///     <see cref="Maybe{TResult}" />.
        /// </summary>
        /// <param name="value">
        ///     The <see cref="Maybe{T}" /> value to transform.
        /// </param>
        /// <param name="transformation">
        ///     The transformation function, which takes a value of type <typeparamref name="T" /> contained in the monad and
        ///     transforms it
        ///     into a new monad of type <see cref="Maybe{TResult}" />.
        /// </param>
        /// <typeparam name="T">
        ///     The type of the value contained in the monad to transform.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type of the value contained in the resulting monad.
        /// </typeparam>
        /// <returns>
        ///     The <see cref="Maybe{TResult}" /> which results from transforming <paramref name="value" /> using
        ///     <paramref name="transformation" />.
        /// </returns>
        public static Maybe<TResult> Bind<T, TResult>(
            this Maybe<T> value,
            [JetBrains.Annotations.InstantHandle] Func<T, Maybe<TResult>> transformation)
        {
            Maybe<TResult> result = value.Match(
                v =>
                {
                    Maybe<TResult> intermediateResult = transformation(v);
                    return intermediateResult;
                },
                () => Maybe.None);

            return result;
        }

        /// <summary>
        ///     The return method for the <see cref="Maybe{T}" /> monad, which encapsulates a value of type
        ///     <typeparamref name="T" /> in an <see cref="Maybe{T}" /> monad.
        /// </summary>
        /// <param name="value">
        ///     The value to encapsulate.
        /// </param>
        /// <typeparam name="T">
        ///     The type of the value.
        /// </typeparam>
        /// <returns>
        ///     The <see cref="Maybe{T}" /> which encapsulates <paramref name="value" />.
        /// </returns>
        public static Maybe<T> Return<T>(T value) => Maybe.Some(value);

        /// <summary>
        ///     Implements the LINQ select many operation for the <see cref="Maybe{T}" /> monad.
        /// </summary>
        /// <param name="value">
        ///     The <see cref="Maybe{T}" /> value to transform.
        /// </param>
        /// <param name="transformation">
        ///     The transformation function, which takes a value of type <typeparamref name="T1" /> contained in the monad and
        ///     transforms it
        ///     into a new monad of type <see cref="Maybe{T2}" />.
        /// </param>
        /// <param name="collation">
        ///     The collation function, which takes a value of type <typeparamref name="T1" /> and a value of type
        ///     <typeparamref name="T2" />
        ///     and combines them into a value of type <typeparamref name="TResult" />.
        /// </param>
        /// <typeparam name="T1">
        ///     The type of the value contained in the monad to transform.
        /// </typeparam>
        /// <typeparam name="T2">
        ///     The type of the value contained in the monad resulting from the transformation.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type of the value contained in the monad resulting from collation.
        /// </typeparam>
        /// <returns>
        ///     The <see cref="Maybe{TResult}" /> which results from transforming <paramref name="value" /> using
        ///     <paramref name="transformation" />
        ///     and then collating using <paramref name="collation" />.
        /// </returns>
        public static Maybe<TResult> SelectMany<T1, T2, TResult>(
            this Maybe<T1> value,
            [JetBrains.Annotations.InstantHandle] Func<T1, Maybe<T2>> transformation,
            [JetBrains.Annotations.InstantHandle] Func<T1, T2, TResult> collation)
        {
            return value.Bind(
                firstValue =>
                {
                    Maybe<T2> result = transformation(firstValue);
                    return result.Bind(secondValue => Maybe.Some(collation(firstValue, secondValue)));
                });
        }

        #endregion
    }
}