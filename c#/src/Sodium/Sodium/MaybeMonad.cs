using System;

namespace Sodium
{
    /// <summary>
    /// Provides methods implementing a monad for <see cref="IMaybe{T}"/>.
    /// </summary>
    public static class MaybeMonad
    {
        #region Public Methods and Operators

        /// <summary>
        /// The bind method for the <see cref="IMaybe{T}"/> monad, which transforms an <see cref="IMaybe{T}"/> into an <see cref="IMaybe{TResult}"/>.
        /// </summary>
        /// <param name="value">
        /// The <see cref="IMaybe{T}"/> value to transform.
        /// </param>
        /// <param name="transformation">
        /// The transformation function, which takes a value of type <typeparamref name="T"/> contained in the monad and transforms it
        /// into a new monad of type <see cref="IMaybe{TResult}"/>.
        /// </param>
        /// <typeparam name="T">
        /// The type of the value contained in the monad to transform.
        /// </typeparam>
        /// <typeparam name="TResult">
        /// The type of the value contained in the resulting monad.
        /// </typeparam>
        /// <returns>
        /// The <see cref="IMaybe{TResult}"/> which results from transforming <paramref name="value"/> using <paramref name="transformation"/>.
        /// </returns>
        public static IMaybe<TResult> Bind<T, TResult>(this IMaybe<T> value, Func<T, IMaybe<TResult>> transformation)
        {
            IMaybe<TResult> result = value.Match(v =>
            {
                IMaybe<TResult> intermediateResult = transformation(v);
                if (intermediateResult == null)
                {
                    throw new InvalidOperationException("The transformation function may not return null.");
                }

                return intermediateResult;
            }, Maybe.Nothing<TResult>);
            if (result == null)
            {
                throw new InvalidOperationException("Result may not be null.");
            }

            return result;
        }

        /// <summary>
        /// The return method for the <see cref="IMaybe{T}"/> monad, which encapsulates a value of type <typeparamref name="T"/> in an <see cref="IMaybe{T}"/> monad.
        /// </summary>
        /// <param name="value">
        /// The value to encapsulate.
        /// </param>
        /// <typeparam name="T">
        /// The type of the value.
        /// </typeparam>
        /// <returns>
        /// The <see cref="IMaybe{T}"/> which encapsulates <paramref name="value"/>.
        /// </returns>
        public static IMaybe<T> Return<T>(T value)
        {
            return Maybe.Just(value);
        }

        /// <summary>
        /// Implements the LINQ select many operation for the <see cref="IMaybe{T}"/> monad.
        /// </summary>
        /// <param name="value">
        /// The <see cref="IMaybe{T}"/> value to transform.
        /// </param>
        /// <param name="transformation">
        /// The transformation function, which takes a value of type <typeparamref name="T1"/> contained in the monad and transforms it
        /// into a new monad of type <see cref="IMaybe{T2}"/>.
        /// </param>
        /// <param name="collation">
        /// The collation function, which takes a value of type <typeparamref name="T1"/> and a value of type <typeparamref name="T2"/>
        /// and combines them into a value of type <typeparamref name="TResult"/>.
        /// </param>
        /// <typeparam name="T1">
        /// The type of the value contained in the monad to transform.
        /// </typeparam>
        /// <typeparam name="T2">
        /// The type of the value contained in the monad resulting from the transformation.
        /// </typeparam>
        /// <typeparam name="TResult">
        /// The type of the value contained in the monad resulting from collation.
        /// </typeparam>
        /// <returns>
        /// The <see cref="IMaybe{TResult}"/> which results from transforming <paramref name="value"/> using <paramref name="transformation"/>
        /// and then collating using <paramref name="collation"/>.
        /// </returns>
        public static IMaybe<TResult> SelectMany<T1, T2, TResult>(this IMaybe<T1> value, Func<T1, IMaybe<T2>> transformation, Func<T1, T2, TResult> collation)
        {
            return value.Bind(firstValue =>
            {
                IMaybe<T2> result = transformation(firstValue);
                if (result == null)
                {
                    throw new InvalidOperationException("The transformation function may not return null.");
                }

                return result.Bind(secondValue => Maybe.Just(collation(firstValue, secondValue)));
            });
        }

        #endregion
    }
}