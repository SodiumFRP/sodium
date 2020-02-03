using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace Sodium.Functional
{
    public static class Maybe
    {
        [JetBrains.Annotations.Pure]
        public static Maybe<T> Some<T>(T value) => Maybe<T>.Some(value);
        
        public static readonly NoneType None = new NoneType();

        public struct NoneType
        {
        }
    }

    public struct Maybe<T> : IMaybe
    {
        private readonly bool hasValue;
        private readonly T value;

        private Maybe(T value)
        {
            this.hasValue = true;
            this.value = value;
        }

        #region Type Constructors

        [JetBrains.Annotations.Pure]
        public static Maybe<T> Some(T value) => new Maybe<T>(value);
        
        public static readonly Maybe<T> None = new Maybe<T>();

        #endregion

        #region Base Functionality

        T1 IMaybe.Match<T1>(Func<object, T1> onSome, Func<T1> onNone) => this.Match(v => onSome(v), onNone);

        public TResult Match<TResult>(Func<T, TResult> onSome, Func<TResult> onNone) =>
            this.hasValue ? onSome(this.value) : onNone();

        #endregion

        #region Helper Methods

        // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
        public void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<T> onSome,
            [JetBrains.Annotations.InstantHandle] Action onNone) =>
            this.Match(onSome.ToFunc(), onNone.ToFunc());

        // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
        public void MatchSome([JetBrains.Annotations.InstantHandle] Action<T> onSome) =>
            this.MatchVoid(onSome, () => { });

        // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
        public void MatchNone([JetBrains.Annotations.InstantHandle] Action onNone) =>
            this.MatchVoid(_ => { }, onNone);

        public Task<TResult> MatchAsync<TResult>(
            [JetBrains.Annotations.InstantHandle] Func<T, Task<TResult>> onSome,
            [JetBrains.Annotations.InstantHandle] Func<Task<TResult>> onNone) =>
            this.Match(onSome, onNone);

        public Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<T, Task> onSome,
            [JetBrains.Annotations.InstantHandle] Func<Task> onNone) =>
            this.MatchAsync(onSome.ToAsyncFunc(), onNone.ToAsyncFunc());

        public Task MatchSomeAsync([JetBrains.Annotations.InstantHandle] Func<T, Task> onSome) =>
            this.MatchAsyncVoid(onSome, () => Task.FromResult(false));
        
        public Task MatchNoneAsync([JetBrains.Annotations.InstantHandle] Func<Task> onNone) =>
            this.MatchAsyncVoid(_ => Task.FromResult(false), onNone);

        /// <summary>
        ///     Map the <see cref="Maybe{T}" /> value using a mapping function if a value exists, or propogate the None value if
        ///     it does not.
        /// </summary>
        /// <param name="f">The function to transform this <see cref="Maybe{T}" />.</param>
        /// <typeparam name="T">The type of the maybe input value.</typeparam>
        /// <typeparam name="TResult">The type of the maybe result value.</typeparam>
        /// <returns>
        ///     The <see cref="Maybe{TResult}" /> which results from transforming this <see cref="Maybe{T}" /> using
        ///     <paramref name="f" />.
        /// </returns>
        public Maybe<TResult> Map<TResult>([JetBrains.Annotations.InstantHandle] Func<T, TResult> f) =>
            this.Bind(v => Maybe.Some(f(v)));

        [JetBrains.Annotations.Pure]
        public bool HasValue() => this.Match(v => true, () => false);

        // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
        void IMaybe.MatchVoid(Action<object> onSome, Action onNone) =>
            this.Upcast<IMaybe>().Match(onSome.ToFunc(), onNone.ToFunc());

        // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
        void IMaybe.MatchSome(Action<object> onSome) => this.Upcast<IMaybe>().MatchVoid(onSome, () => { });

        // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
        void IMaybe.MatchNone(Action onNone) => this.Upcast<IMaybe>().MatchVoid(_ => { }, onNone);

        Task<TResult> IMaybe.MatchAsync<TResult>(Func<object, Task<TResult>> onSome, Func<Task<TResult>> onNone) =>
            this.Upcast<IMaybe>().Match(onSome, onNone);

        Task IMaybe.MatchAsyncVoid(Func<object, Task> onSome, Func<Task> onNone) =>
            this.Upcast<IMaybe>().MatchAsync(onSome.ToAsyncFunc(), onNone.ToAsyncFunc());

        Task IMaybe.MatchSomeAsync(Func<object, Task> onSome) =>
            this.Upcast<IMaybe>().MatchAsyncVoid(onSome, () => Task.FromResult(false));

        Task IMaybe.MatchNoneAsync(Func<Task> onNone) =>
            this.Upcast<IMaybe>().MatchAsyncVoid(_ => Task.FromResult(false), onNone);

        #endregion

        public static implicit operator Maybe<T>(Maybe.NoneType _) => None;

        public static bool operator ==(Maybe<T> x, Maybe<T> y) =>
            x.hasValue == y.hasValue && EqualityComparer<T>.Default.Equals(x.value, y.value);

        public static bool operator !=(Maybe<T> x, Maybe<T> y) => !(x == y);
        public override bool Equals(object obj) => obj is Maybe<T> m && this == m;

        public override int GetHashCode() =>
            (this.hasValue.GetHashCode() * 397) ^ EqualityComparer<T>.Default.GetHashCode(this.value);

        public override string ToString() => this.Match(v => $"{{Some: {v}}}", () => "{None}");
    }
}