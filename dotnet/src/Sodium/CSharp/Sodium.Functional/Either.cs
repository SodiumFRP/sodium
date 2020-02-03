using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace Sodium.Functional
{
    public static class Either
    {
        public static EitherFirst<T> First<T>(T value) => new EitherFirst<T>(value);
        public static EitherSecond<T> Second<T>(T value) => new EitherSecond<T>(value);
        public static EitherThird<T> Third<T>(T value) => new EitherThird<T>(value);
        public static EitherFourth<T> Fourth<T>(T value) => new EitherFourth<T>(value);
        public static EitherFifth<T> Fifth<T>(T value) => new EitherFifth<T>(value);
        public static EitherSixth<T> Sixth<T>(T value) => new EitherSixth<T>(value);
        public static EitherSeventh<T> Seventh<T>(T value) => new EitherSeventh<T>(value);
        public static EitherEighth<T> Eighth<T>(T value) => new EitherEighth<T>(value);

        public static GetValueAsHelper<T> GetValueAs<T>() => GetValueAsHelper<T>.Instance;

        public sealed class EitherFirst<T>
        {
            internal EitherFirst(T value) => this.Value = value;

            internal T Value { get; }
        }

        public sealed class EitherSecond<T>
        {
            internal EitherSecond(T value) => this.Value = value;

            internal T Value { get; }
        }

        public sealed class EitherThird<T>
        {
            internal EitherThird(T value) => this.Value = value;

            internal T Value { get; }
        }

        public sealed class EitherFourth<T>
        {
            internal EitherFourth(T value) => this.Value = value;

            internal T Value { get; }
        }

        public sealed class EitherFifth<T>
        {
            internal EitherFifth(T value) => this.Value = value;

            internal T Value { get; }
        }

        public sealed class EitherSixth<T>
        {
            internal EitherSixth(T value) => this.Value = value;

            internal T Value { get; }
        }

        public sealed class EitherSeventh<T>
        {
            internal EitherSeventh(T value) => this.Value = value;

            internal T Value { get; }
        }

        public sealed class EitherEighth<T>
        {
            internal EitherEighth(T value) => this.Value = value;

            internal T Value { get; }
        }

        public class GetValueAsHelper<T>
        {
            internal static readonly GetValueAsHelper<T> Instance = new GetValueAsHelper<T>();

            private GetValueAsHelper()
            {
            }

            public T From<T1, T2>(Either<T1, T2> a)
                where T1 : T
                where T2 : T =>
                a.Match<T>(v1 => v1, v2 => v2);

            public T From<T1, T2, T3>(Either<T1, T2, T3> a)
                where T1 : T
                where T2 : T
                where T3 : T =>
                a.Match<T>(v1 => v1, v2 => v2, v3 => v3);

            public T From<T1, T2, T3, T4>(Either<T1, T2, T3, T4> a)
                where T1 : T
                where T2 : T
                where T3 : T
                where T4 : T =>
                a.Match<T>(v1 => v1, v2 => v2, v3 => v3, v4 => v4);

            public T From<T1, T2, T3, T4, T5>(Either<T1, T2, T3, T4, T5> a)
                where T1 : T
                where T2 : T
                where T3 : T
                where T4 : T
                where T5 : T =>
                a.Match<T>(v1 => v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5);

            public T From<T1, T2, T3, T4, T5, T6>(Either<T1, T2, T3, T4, T5, T6> a)
                where T1 : T
                where T2 : T
                where T3 : T
                where T4 : T
                where T5 : T
                where T6 : T =>
                a.Match<T>(v1 => v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5, v6 => v6);

            public T From<T1, T2, T3, T4, T5, T6, T7>(Either<T1, T2, T3, T4, T5, T6, T7> a)
                where T1 : T
                where T2 : T
                where T3 : T
                where T4 : T
                where T5 : T
                where T6 : T
                where T7 : T =>
                a.Match<T>(v1 => v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5, v6 => v6, v7 => v7);

            public T From<T1, T2, T3, T4, T5, T6, T7, T8>(Either<T1, T2, T3, T4, T5, T6, T7, T8> a)
                where T1 : T
                where T2 : T
                where T3 : T
                where T4 : T
                where T5 : T
                where T6 : T
                where T7 : T
                where T8 : T =>
                a.Match<T>(v1 => v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5, v6 => v6, v7 => v7, v8 => v8);
        }
    }
    
    public struct Either<T1, T2> : IEitherOfTwo
    {
        private readonly int valueType;
        private readonly T1 value1;
        private readonly T2 value2;

        private Either(int valueType, T1 value1, T2 value2)
        {
            this.valueType = valueType;
            this.value1 = value1;
            this.value2 = value2;
        }

        #region Type Constructors

        public static Either<T1, T2> First(T1 value) => new Either<T1, T2>(0, value, default(T2));
        public static Either<T1, T2> Second(T2 value) => new Either<T1, T2>(1, default(T1), value);

        #endregion

        #region Base Functionality

        T IEitherOfTwo.Match<T>(Func<object, T> onFirst, Func<object, T> onSecond) =>
            this.Match(v => onFirst(v), v => onSecond(v));

        object IEither.GetValueAsObject() => Either.GetValueAs<object>().From(this);

        public T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, T> onSecond) =>
            this.valueType == 0 ? onFirst(this.value1) : onSecond(this.value2);

        #endregion

        #region Helper Methods

        public void MatchVoid(Action<T1> onFirst, Action<T2> onSecond) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Match(onFirst.ToFunc(), onSecond.ToFunc());

        public Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task<T>> onSecond) =>
            this.Match(onFirst, onSecond);

        public Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task> onSecond) =>
            this.MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc());

        public Either<T, T2> MapFirst<T>([JetBrains.Annotations.InstantHandle] Func<T1, T> f) =>
            this.Match(v1 => Either<T, T2>.First(f(v1)), v2 => Either.Second(v2));

        public Either<T1, T> MapSecond<T>([JetBrains.Annotations.InstantHandle] Func<T2, T> f) =>
            this.Match(Either<T1, T>.First, v2 => Either.Second(f(v2)));

        [JetBrains.Annotations.Pure]
        public Maybe<T1> TryGetFirst() =>
            this.Match(Maybe.Some, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T2> TryGetSecond() =>
            this.Match(_ => Maybe.None, Maybe.Some);

        [JetBrains.Annotations.Pure]
        public bool IsFirst() =>
            this.Match(_ => true, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSecond() =>
            this.Match(_ => false, _ => true);

        void IEitherOfTwo.MatchVoid(Action<object> onFirst, Action<object> onSecond) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Upcast<IEitherOfTwo>().Match(onFirst.ToFunc(), onSecond.ToFunc());

        Task<T> IEitherOfTwo.MatchAsync<T>(Func<object, Task<T>> onFirst, Func<object, Task<T>> onSecond) =>
            this.Upcast<IEitherOfTwo>().Match(onFirst, onSecond);

        Task IEitherOfTwo.MatchAsyncVoid(Func<object, Task> onFirst, Func<object, Task> onSecond) =>
            this.Upcast<IEitherOfTwo>().MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc());

        #endregion

        public static implicit operator Either<T1, T2>(Either.EitherFirst<T1> value) =>
            First(value == null ? default(T1) : value.Value);

        public static implicit operator Either<T1, T2>(Either.EitherSecond<T2> value) =>
            Second(value == null ? default(T2) : value.Value);

        public static bool operator ==(Either<T1, T2> x, Either<T1, T2> y) =>
            x.valueType == y.valueType
            && EqualityComparer<T1>.Default.Equals(x.value1, y.value1)
            && EqualityComparer<T2>.Default.Equals(x.value2, y.value2);

        public static bool operator !=(Either<T1, T2> x, Either<T1, T2> y) => !(x == y);
        public override bool Equals(object obj) => obj is Either<T1, T2> e && this == e;

        public override int GetHashCode()
        {
            unchecked
            {
                int hashCode = this.valueType;
                hashCode = (hashCode * 397) ^ EqualityComparer<T1>.Default.GetHashCode(this.value1);
                hashCode = (hashCode * 397) ^ EqualityComparer<T2>.Default.GetHashCode(this.value2);
                return hashCode;
            }
        }

        public override string ToString() => this.Match(v1 => $"First: {v1}", v2 => $"Second: {v2}");
    }

    public struct Either<T1, T2, T3> : IEitherOfThree
    {
        private readonly int valueType;
        private readonly T1 value1;
        private readonly T2 value2;
        private readonly T3 value3;

        private Either(int valueType, T1 value1, T2 value2, T3 value3)
        {
            this.valueType = valueType;
            this.value1 = value1;
            this.value2 = value2;
            this.value3 = value3;
        }

        #region Type Constructors

        public static Either<T1, T2, T3> First(T1 value) => new Either<T1, T2, T3>(0, value, default(T2), default(T3));
        public static Either<T1, T2, T3> Second(T2 value) => new Either<T1, T2, T3>(1, default(T1), value, default(T3));
        public static Either<T1, T2, T3> Third(T3 value) => new Either<T1, T2, T3>(2, default(T1), default(T2), value);

        #endregion

        #region Base Functionality

        T IEitherOfThree.Match<T>(Func<object, T> onFirst, Func<object, T> onSecond, Func<object, T> onThird) =>
            this.Match(v => onFirst(v), v => onSecond(v), v => onThird(v));

        object IEither.GetValueAsObject() => Either.GetValueAs<object>().From(this);

        public T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, T> onThird) =>
            this.valueType == 0
                ? onFirst(this.value1)
                : (this.valueType == 1 ? onSecond(this.value2) : onThird(this.value3));

        #endregion

        #region Helper Methods

        public void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<T1> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<T2> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<T3> onThird) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc());

        public Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task<T>> onThird) =>
            this.Match(onFirst, onSecond, onThird);

        public Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task> onThird) =>
            this.MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc(), onThird.ToAsyncFunc());

        public Either<T, T2, T3> MapFirst<T>([JetBrains.Annotations.InstantHandle] Func<T1, T> f) =>
            this.Match(v1 => Either<T, T2, T3>.First(f(v1)), v2 => Either.Second(v2), v3 => Either.Third(v3));

        public Either<T1, T, T3> MapSecond<T>([JetBrains.Annotations.InstantHandle] Func<T2, T> f) =>
            this.Match(Either<T1, T, T3>.First, v2 => Either.Second(f(v2)), v3 => Either.Third(v3));

        public Either<T1, T2, T> MapThird<T>([JetBrains.Annotations.InstantHandle] Func<T3, T> f) =>
            this.Match(Either<T1, T2, T>.First, v2 => Either.Second(v2), v3 => Either.Third(f(v3)));

        [JetBrains.Annotations.Pure]
        public Maybe<T1> TryGetFirst() =>
            this.Match(Maybe.Some, _ => Maybe.None, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T2> TryGetSecond() =>
            this.Match(_ => Maybe.None, Maybe.Some, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T3> TryGetThird() =>
            this.Match(_ => Maybe.None, _ => Maybe.None, Maybe.Some);

        [JetBrains.Annotations.Pure]
        public bool IsFirst() =>
            this.Match(_ => true, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSecond() =>
            this.Match(_ => false, _ => true, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsThird() =>
            this.Match(_ => false, _ => false, _ => true);

        void IEitherOfThree.MatchVoid(Action<object> onFirst, Action<object> onSecond, Action<object> onThird) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Upcast<IEitherOfThree>().Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc());

        Task<T> IEitherOfThree.MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird) =>
            this.Upcast<IEitherOfThree>().Match(onFirst, onSecond, onThird);

        Task IEitherOfThree.MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird) =>
            this.Upcast<IEitherOfThree>()
                .MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc(), onThird.ToAsyncFunc());

        #endregion

        public static implicit operator Either<T1, T2, T3>(Either.EitherFirst<T1> value) =>
            First(value == null ? default(T1) : value.Value);

        public static implicit operator Either<T1, T2, T3>(Either.EitherSecond<T2> value) =>
            Second(value == null ? default(T2) : value.Value);

        public static implicit operator Either<T1, T2, T3>(Either.EitherThird<T3> value) =>
            Third(value == null ? default(T3) : value.Value);

        public static bool operator ==(Either<T1, T2, T3> x, Either<T1, T2, T3> y) =>
            x.valueType == y.valueType
            && EqualityComparer<T1>.Default.Equals(x.value1, y.value1)
            && EqualityComparer<T2>.Default.Equals(x.value2, y.value2)
            && EqualityComparer<T3>.Default.Equals(x.value3, y.value3);

        public static bool operator !=(Either<T1, T2, T3> x, Either<T1, T2, T3> y) => !(x == y);
        public override bool Equals(object obj) => obj is Either<T1, T2, T3> e && this == e;

        public override int GetHashCode()
        {
            unchecked
            {
                int hashCode = this.valueType;
                hashCode = (hashCode * 397) ^ EqualityComparer<T1>.Default.GetHashCode(this.value1);
                hashCode = (hashCode * 397) ^ EqualityComparer<T2>.Default.GetHashCode(this.value2);
                hashCode = (hashCode * 397) ^ EqualityComparer<T3>.Default.GetHashCode(this.value3);
                return hashCode;
            }
        }

        public override string ToString() => this.Match(
            v1 => $"First: {v1}",
            v2 => $"Second: {v2}",
            v3 => $"Third: {v3}");
    }

    public struct Either<T1, T2, T3, T4> : IEitherOfFour
    {
        private readonly int valueType;
        private readonly T1 value1;
        private readonly T2 value2;
        private readonly T3 value3;
        private readonly T4 value4;

        private Either(int valueType, T1 value1, T2 value2, T3 value3, T4 value4)
        {
            this.valueType = valueType;
            this.value1 = value1;
            this.value2 = value2;
            this.value3 = value3;
            this.value4 = value4;
        }

        #region Type Constructors

        public static Either<T1, T2, T3, T4> First(T1 value) =>
            new Either<T1, T2, T3, T4>(0, value, default(T2), default(T3), default(T4));

        public static Either<T1, T2, T3, T4> Second(T2 value) =>
            new Either<T1, T2, T3, T4>(1, default(T1), value, default(T3), default(T4));

        public static Either<T1, T2, T3, T4> Third(T3 value) =>
            new Either<T1, T2, T3, T4>(2, default(T1), default(T2), value, default(T4));

        public static Either<T1, T2, T3, T4> Fourth(T4 value) =>
            new Either<T1, T2, T3, T4>(3, default(T1), default(T2), default(T3), value);

        #endregion

        #region Base Functionality

        T IEitherOfFour.Match<T>(
            Func<object, T> onFirst,
            Func<object, T> onSecond,
            Func<object, T> onThird,
            Func<object, T> onFourth) =>
            this.Match(v => onFirst(v), v => onSecond(v), v => onThird(v), v => onFourth(v));

        object IEither.GetValueAsObject() => Either.GetValueAs<object>().From(this);

        public T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, T> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, T> onFourth) =>
            this.valueType == 0
                ? onFirst(this.value1)
                : (this.valueType == 1
                    ? onSecond(this.value2)
                    : (this.valueType == 2 ? onThird(this.value3) : onFourth(this.value4)));

        #endregion

        #region Helper Methods

        public void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<T1> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<T2> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<T3> onThird,
            [JetBrains.Annotations.InstantHandle] Action<T4> onFourth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc(), onFourth.ToFunc());

        public Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task<T>> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, Task<T>> onFourth) =>
            this.Match(onFirst, onSecond, onThird, onFourth);

        public Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, Task> onFourth) =>
            this.MatchAsync(
                onFirst.ToAsyncFunc(),
                onSecond.ToAsyncFunc(),
                onThird.ToAsyncFunc(),
                onFourth.ToAsyncFunc());

        public Either<T, T2, T3, T4> MapFirst<T>([JetBrains.Annotations.InstantHandle] Func<T1, T> f) =>
            this.Match(
                v1 => Either<T, T2, T3, T4>.First(f(v1)),
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4));

        public Either<T1, T, T3, T4> MapSecond<T>([JetBrains.Annotations.InstantHandle] Func<T2, T> f) =>
            this.Match(
                Either<T1, T, T3, T4>.First,
                v2 => Either.Second(f(v2)),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4));

        public Either<T1, T2, T, T4> MapThird<T>([JetBrains.Annotations.InstantHandle] Func<T3, T> f) =>
            this.Match(
                Either<T1, T2, T, T4>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(f(v3)),
                v4 => Either.Fourth(v4));

        public Either<T1, T2, T3, T> MapFourth<T>([JetBrains.Annotations.InstantHandle] Func<T4, T> f) =>
            this.Match(
                Either<T1, T2, T3, T>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(f(v4)));

        [JetBrains.Annotations.Pure]
        public Maybe<T1> TryGetFirst() =>
            this.Match(Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T2> TryGetSecond() =>
            this.Match(_ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T3> TryGetThird() =>
            this.Match(_ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T4> TryGetFourth() =>
            this.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some);

        [JetBrains.Annotations.Pure]
        public bool IsFirst() =>
            this.Match(_ => true, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSecond() =>
            this.Match(_ => false, _ => true, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsThird() =>
            this.Match(_ => false, _ => false, _ => true, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsFourth() =>
            this.Match(_ => false, _ => false, _ => false, _ => true);

        void IEitherOfFour.MatchVoid(
            Action<object> onFirst,
            Action<object> onSecond,
            Action<object> onThird,
            Action<object> onFourth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Upcast<IEitherOfFour>()
                .Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc(), onFourth.ToFunc());

        Task<T> IEitherOfFour.MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird,
            Func<object, Task<T>> onFourth) =>
            this.Upcast<IEitherOfFour>().Match(onFirst, onSecond, onThird, onFourth);

        Task IEitherOfFour.MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird,
            Func<object, Task> onFourth) =>
            this.Upcast<IEitherOfFour>()
                .MatchAsync(
                    onFirst.ToAsyncFunc(),
                    onSecond.ToAsyncFunc(),
                    onThird.ToAsyncFunc(),
                    onFourth.ToAsyncFunc());

        #endregion

        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherFirst<T1> value) =>
            First(value == null ? default(T1) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherSecond<T2> value) =>
            Second(value == null ? default(T2) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherThird<T3> value) =>
            Third(value == null ? default(T3) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherFourth<T4> value) =>
            Fourth(value == null ? default(T4) : value.Value);

        public static bool operator ==(Either<T1, T2, T3, T4> x, Either<T1, T2, T3, T4> y) =>
            x.valueType == y.valueType
            && EqualityComparer<T1>.Default.Equals(x.value1, y.value1)
            && EqualityComparer<T2>.Default.Equals(x.value2, y.value2)
            && EqualityComparer<T3>.Default.Equals(x.value3, y.value3)
            && EqualityComparer<T4>.Default.Equals(x.value4, y.value4);

        public static bool operator !=(Either<T1, T2, T3, T4> x, Either<T1, T2, T3, T4> y) => !(x == y);
        public override bool Equals(object obj) => obj is Either<T1, T2, T3, T4> e && this == e;

        public override int GetHashCode()
        {
            unchecked
            {
                int hashCode = this.valueType;
                hashCode = (hashCode * 397) ^ EqualityComparer<T1>.Default.GetHashCode(this.value1);
                hashCode = (hashCode * 397) ^ EqualityComparer<T2>.Default.GetHashCode(this.value2);
                hashCode = (hashCode * 397) ^ EqualityComparer<T3>.Default.GetHashCode(this.value3);
                hashCode = (hashCode * 397) ^ EqualityComparer<T4>.Default.GetHashCode(this.value4);
                return hashCode;
            }
        }

        public override string ToString() => this.Match(
            v1 => $"First: {v1}",
            v2 => $"Second: {v2}",
            v3 => $"Third: {v3}",
            v4 => $"Fourth: {v4}");
    }

    public struct Either<T1, T2, T3, T4, T5> : IEitherOfFive
    {
        private readonly int valueType;
        private readonly T1 value1;
        private readonly T2 value2;
        private readonly T3 value3;
        private readonly T4 value4;
        private readonly T5 value5;

        private Either(int valueType, T1 value1, T2 value2, T3 value3, T4 value4, T5 value5)
        {
            this.valueType = valueType;
            this.value1 = value1;
            this.value2 = value2;
            this.value3 = value3;
            this.value4 = value4;
            this.value5 = value5;
        }

        #region Type Constructors

        public static Either<T1, T2, T3, T4, T5> First(T1 value) => new Either<T1, T2, T3, T4, T5>(
            0,
            value,
            default(T2),
            default(T3),
            default(T4),
            default(T5));

        public static Either<T1, T2, T3, T4, T5> Second(T2 value) => new Either<T1, T2, T3, T4, T5>(
            1,
            default(T1),
            value,
            default(T3),
            default(T4),
            default(T5));

        public static Either<T1, T2, T3, T4, T5> Third(T3 value) => new Either<T1, T2, T3, T4, T5>(
            2,
            default(T1),
            default(T2),
            value,
            default(T4),
            default(T5));

        public static Either<T1, T2, T3, T4, T5> Fourth(T4 value) => new Either<T1, T2, T3, T4, T5>(
            3,
            default(T1),
            default(T2),
            default(T3),
            value,
            default(T5));

        public static Either<T1, T2, T3, T4, T5> Fifth(T5 value) => new Either<T1, T2, T3, T4, T5>(
            4,
            default(T1),
            default(T2),
            default(T3),
            default(T4),
            value);

        #endregion

        #region Base Functionality

        T IEitherOfFive.Match<T>(
            Func<object, T> onFirst,
            Func<object, T> onSecond,
            Func<object, T> onThird,
            Func<object, T> onFourth,
            Func<object, T> onFifth) =>
            this.Match(v => onFirst(v), v => onSecond(v), v => onThird(v), v => onFourth(v), v => onFifth(v));

        object IEither.GetValueAsObject() => Either.GetValueAs<object>().From(this);

        public T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, T> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, T> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, T> onFifth) =>
            this.valueType == 0
                ? onFirst(this.value1)
                : (this.valueType == 1
                    ? onSecond(this.value2)
                    : (this.valueType == 2
                        ? onThird(this.value3)
                        : (this.valueType == 3 ? onFourth(this.value4) : onFifth(this.value5))));

        #endregion

        #region Helper Methods

        public void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<T1> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<T2> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<T3> onThird,
            [JetBrains.Annotations.InstantHandle] Action<T4> onFourth,
            [JetBrains.Annotations.InstantHandle] Action<T5> onFifth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc(), onFourth.ToFunc(), onFifth.ToFunc());

        public Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task<T>> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, Task<T>> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, Task<T>> onFifth) =>
            this.Match(onFirst, onSecond, onThird, onFourth, onFifth);

        public Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, Task> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, Task> onFifth) =>
            this.MatchAsync(
                onFirst.ToAsyncFunc(),
                onSecond.ToAsyncFunc(),
                onThird.ToAsyncFunc(),
                onFourth.ToAsyncFunc(),
                onFifth.ToAsyncFunc());

        public Either<T, T2, T3, T4, T5> MapFirst<T>([JetBrains.Annotations.InstantHandle] Func<T1, T> f) =>
            this.Match(
                v1 => Either<T, T2, T3, T4, T5>.First(f(v1)),
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5));

        public Either<T1, T, T3, T4, T5> MapSecond<T>([JetBrains.Annotations.InstantHandle] Func<T2, T> f) =>
            this.Match(
                Either<T1, T, T3, T4, T5>.First,
                v2 => Either.Second(f(v2)),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5));

        public Either<T1, T2, T, T4, T5> MapThird<T>([JetBrains.Annotations.InstantHandle] Func<T3, T> f) =>
            this.Match(
                Either<T1, T2, T, T4, T5>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(f(v3)),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5));

        public Either<T1, T2, T3, T, T5> MapFourth<T>([JetBrains.Annotations.InstantHandle] Func<T4, T> f) =>
            this.Match(
                Either<T1, T2, T3, T, T5>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(f(v4)),
                v5 => Either.Fifth(v5));

        public Either<T1, T2, T3, T4, T> MapFifth<T>([JetBrains.Annotations.InstantHandle] Func<T5, T> f) =>
            this.Match(
                Either<T1, T2, T3, T4, T>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(f(v5)));

        [JetBrains.Annotations.Pure]
        public Maybe<T1> TryGetFirst() =>
            this.Match(Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T2> TryGetSecond() =>
            this.Match(_ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T3> TryGetThird() =>
            this.Match(_ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T4> TryGetFourth() =>
            this.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T5> TryGetFifth() =>
            this.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some);

        [JetBrains.Annotations.Pure]
        public bool IsFirst() =>
            this.Match(_ => true, _ => false, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSecond() =>
            this.Match(_ => false, _ => true, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsThird() =>
            this.Match(_ => false, _ => false, _ => true, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsFourth() =>
            this.Match(_ => false, _ => false, _ => false, _ => true, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsFifth() =>
            this.Match(_ => false, _ => false, _ => false, _ => false, _ => true);

        void IEitherOfFive.MatchVoid(
            Action<object> onFirst,
            Action<object> onSecond,
            Action<object> onThird,
            Action<object> onFourth,
            Action<object> onFifth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Upcast<IEitherOfFive>()
                .Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc(), onFourth.ToFunc(), onFifth.ToFunc());

        Task<T> IEitherOfFive.MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird,
            Func<object, Task<T>> onFourth,
            Func<object, Task<T>> onFifth) =>
            this.Upcast<IEitherOfFive>().Match(onFirst, onSecond, onThird, onFourth, onFifth);

        Task IEitherOfFive.MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird,
            Func<object, Task> onFourth,
            Func<object, Task> onFifth) =>
            this.Upcast<IEitherOfFive>()
                .MatchAsync(
                    onFirst.ToAsyncFunc(),
                    onSecond.ToAsyncFunc(),
                    onThird.ToAsyncFunc(),
                    onFourth.ToAsyncFunc(),
                    onFifth.ToAsyncFunc());

        #endregion

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherFirst<T1> value) =>
            First(value == null ? default(T1) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherSecond<T2> value) =>
            Second(value == null ? default(T2) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherThird<T3> value) =>
            Third(value == null ? default(T3) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherFourth<T4> value) =>
            Fourth(value == null ? default(T4) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherFifth<T5> value) =>
            Fifth(value == null ? default(T5) : value.Value);

        public static bool operator ==(Either<T1, T2, T3, T4, T5> x, Either<T1, T2, T3, T4, T5> y) =>
            x.valueType == y.valueType
            && EqualityComparer<T1>.Default.Equals(x.value1, y.value1)
            && EqualityComparer<T2>.Default.Equals(x.value2, y.value2)
            && EqualityComparer<T3>.Default.Equals(x.value3, y.value3)
            && EqualityComparer<T4>.Default.Equals(x.value4, y.value4)
            && EqualityComparer<T5>.Default.Equals(x.value5, y.value5);

        public static bool operator !=(Either<T1, T2, T3, T4, T5> x, Either<T1, T2, T3, T4, T5> y) => !(x == y);
        public override bool Equals(object obj) => obj is Either<T1, T2, T3, T4, T5> e && this == e;

        public override int GetHashCode()
        {
            unchecked
            {
                int hashCode = this.valueType;
                hashCode = (hashCode * 397) ^ EqualityComparer<T1>.Default.GetHashCode(this.value1);
                hashCode = (hashCode * 397) ^ EqualityComparer<T2>.Default.GetHashCode(this.value2);
                hashCode = (hashCode * 397) ^ EqualityComparer<T3>.Default.GetHashCode(this.value3);
                hashCode = (hashCode * 397) ^ EqualityComparer<T4>.Default.GetHashCode(this.value4);
                hashCode = (hashCode * 397) ^ EqualityComparer<T5>.Default.GetHashCode(this.value5);
                return hashCode;
            }
        }

        public override string ToString() => this.Match(
            v1 => $"First: {v1}",
            v2 => $"Second: {v2}",
            v3 => $"Third: {v3}",
            v4 => $"Fourth: {v4}",
            v5 => $"Fifth: {v5}");
    }

    public struct Either<T1, T2, T3, T4, T5, T6> : IEitherOfSix
    {
        private readonly int valueType;
        private readonly T1 value1;
        private readonly T2 value2;
        private readonly T3 value3;
        private readonly T4 value4;
        private readonly T5 value5;
        private readonly T6 value6;

        internal Either(int valueType, T1 value1, T2 value2, T3 value3, T4 value4, T5 value5, T6 value6)
        {
            this.valueType = valueType;
            this.value1 = value1;
            this.value2 = value2;
            this.value3 = value3;
            this.value4 = value4;
            this.value5 = value5;
            this.value6 = value6;
        }

        #region Type Constructors

        public static Either<T1, T2, T3, T4, T5, T6> First(T1 value) => new Either<T1, T2, T3, T4, T5, T6>(
            0,
            value,
            default(T2),
            default(T3),
            default(T4),
            default(T5),
            default(T6));

        public static Either<T1, T2, T3, T4, T5, T6> Second(T2 value) => new Either<T1, T2, T3, T4, T5, T6>(
            1,
            default(T1),
            value,
            default(T3),
            default(T4),
            default(T5),
            default(T6));

        public static Either<T1, T2, T3, T4, T5, T6> Third(T3 value) => new Either<T1, T2, T3, T4, T5, T6>(
            2,
            default(T1),
            default(T2),
            value,
            default(T4),
            default(T5),
            default(T6));

        public static Either<T1, T2, T3, T4, T5, T6> Fourth(T4 value) => new Either<T1, T2, T3, T4, T5, T6>(
            3,
            default(T1),
            default(T2),
            default(T3),
            value,
            default(T5),
            default(T6));

        public static Either<T1, T2, T3, T4, T5, T6> Fifth(T5 value) => new Either<T1, T2, T3, T4, T5, T6>(
            4,
            default(T1),
            default(T2),
            default(T3),
            default(T4),
            value,
            default(T6));

        public static Either<T1, T2, T3, T4, T5, T6> Sixth(T6 value) => new Either<T1, T2, T3, T4, T5, T6>(
            5,
            default(T1),
            default(T2),
            default(T3),
            default(T4),
            default(T5),
            value);

        #endregion

        #region Base Functionality

        T IEitherOfSix.Match<T>(
            Func<object, T> onFirst,
            Func<object, T> onSecond,
            Func<object, T> onThird,
            Func<object, T> onFourth,
            Func<object, T> onFifth,
            Func<object, T> onSixth) =>
            this.Match(
                v => onFirst(v),
                v => onSecond(v),
                v => onThird(v),
                v => onFourth(v),
                v => onFifth(v),
                v => onSixth(v));

        object IEither.GetValueAsObject() => Either.GetValueAs<object>().From(this);

        public T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, T> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, T> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, T> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<T6, T> onSixth) =>
            this.valueType == 0
                ? onFirst(this.value1)
                : (this.valueType == 1
                    ? onSecond(this.value2)
                    : (this.valueType == 2
                        ? onThird(this.value3)
                        : (this.valueType == 3
                            ? onFourth(this.value4)
                            : (this.valueType == 4 ? onFifth(this.value5) : onSixth(this.value6)))));

        #endregion

        #region Helper Methods

        public void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<T1> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<T2> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<T3> onThird,
            [JetBrains.Annotations.InstantHandle] Action<T4> onFourth,
            [JetBrains.Annotations.InstantHandle] Action<T5> onFifth,
            [JetBrains.Annotations.InstantHandle] Action<T6> onSixth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Match(
                onFirst.ToFunc(),
                onSecond.ToFunc(),
                onThird.ToFunc(),
                onFourth.ToFunc(),
                onFifth.ToFunc(),
                onSixth.ToFunc());

        public Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task<T>> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, Task<T>> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, Task<T>> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<T6, Task<T>> onSixth) =>
            this.Match(onFirst, onSecond, onThird, onFourth, onFifth, onSixth);

        public Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, Task> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, Task> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<T6, Task> onSixth) =>
            this.MatchAsync(
                onFirst.ToAsyncFunc(),
                onSecond.ToAsyncFunc(),
                onThird.ToAsyncFunc(),
                onFourth.ToAsyncFunc(),
                onFifth.ToAsyncFunc(),
                onSixth.ToAsyncFunc());

        public Either<T, T2, T3, T4, T5, T6> MapFirst<T>([JetBrains.Annotations.InstantHandle] Func<T1, T> f) =>
            this.Match(
                v1 => Either<T, T2, T3, T4, T5, T6>.First(f(v1)),
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6));

        public Either<T1, T, T3, T4, T5, T6> MapSecond<T>([JetBrains.Annotations.InstantHandle] Func<T2, T> f) =>
            this.Match(
                Either<T1, T, T3, T4, T5, T6>.First,
                v2 => Either.Second(f(v2)),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6));

        public Either<T1, T2, T, T4, T5, T6> MapThird<T>([JetBrains.Annotations.InstantHandle] Func<T3, T> f) =>
            this.Match(
                Either<T1, T2, T, T4, T5, T6>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(f(v3)),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6));

        public Either<T1, T2, T3, T, T5, T6> MapFourth<T>([JetBrains.Annotations.InstantHandle] Func<T4, T> f) =>
            this.Match(
                Either<T1, T2, T3, T, T5, T6>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(f(v4)),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6));

        public Either<T1, T2, T3, T4, T, T6> MapFifth<T>([JetBrains.Annotations.InstantHandle] Func<T5, T> f) =>
            this.Match(
                Either<T1, T2, T3, T4, T, T6>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(f(v5)),
                v6 => Either.Sixth(v6));

        public Either<T1, T2, T3, T4, T5, T> MapSixth<T>([JetBrains.Annotations.InstantHandle] Func<T6, T> f) =>
            this.Match(
                Either<T1, T2, T3, T4, T5, T>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(f(v6)));

        [JetBrains.Annotations.Pure]
        public Maybe<T1> TryGetFirst() =>
            this.Match(Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T2> TryGetSecond() =>
            this.Match(_ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T3> TryGetThird() =>
            this.Match(_ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T4> TryGetFourth() =>
            this.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T5> TryGetFifth() =>
            this.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T6> TryGetSixth() =>
            this.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some);

        [JetBrains.Annotations.Pure]
        public bool IsFirst() =>
            this.Match(_ => true, _ => false, _ => false, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSecond() =>
            this.Match(_ => false, _ => true, _ => false, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsThird() =>
            this.Match(_ => false, _ => false, _ => true, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsFourth() =>
            this.Match(_ => false, _ => false, _ => false, _ => true, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsFifth() =>
            this.Match(_ => false, _ => false, _ => false, _ => false, _ => true, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSixth() =>
            this.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => true);

        void IEitherOfSix.MatchVoid(
            Action<object> onFirst,
            Action<object> onSecond,
            Action<object> onThird,
            Action<object> onFourth,
            Action<object> onFifth,
            Action<object> onSixth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Upcast<IEitherOfSix>()
                .Match(
                    onFirst.ToFunc(),
                    onSecond.ToFunc(),
                    onThird.ToFunc(),
                    onFourth.ToFunc(),
                    onFifth.ToFunc(),
                    onSixth.ToFunc());

        Task<T> IEitherOfSix.MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird,
            Func<object, Task<T>> onFourth,
            Func<object, Task<T>> onFifth,
            Func<object, Task<T>> onSixth) =>
            this.Upcast<IEitherOfSix>().Match(onFirst, onSecond, onThird, onFourth, onFifth, onSixth);

        Task IEitherOfSix.MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird,
            Func<object, Task> onFourth,
            Func<object, Task> onFifth,
            Func<object, Task> onSixth) =>
            this.Upcast<IEitherOfSix>()
                .MatchAsync(
                    onFirst.ToAsyncFunc(),
                    onSecond.ToAsyncFunc(),
                    onThird.ToAsyncFunc(),
                    onFourth.ToAsyncFunc(),
                    onFifth.ToAsyncFunc(),
                    onSixth.ToAsyncFunc());

        #endregion

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherFirst<T1> value) =>
            First(value == null ? default(T1) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherSecond<T2> value) =>
            Second(value == null ? default(T2) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherThird<T3> value) =>
            Third(value == null ? default(T3) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherFourth<T4> value) =>
            Fourth(value == null ? default(T4) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherFifth<T5> value) =>
            Fifth(value == null ? default(T5) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherSixth<T6> value) =>
            Sixth(value == null ? default(T6) : value.Value);

        public static bool operator ==(Either<T1, T2, T3, T4, T5, T6> x, Either<T1, T2, T3, T4, T5, T6> y) =>
            x.valueType == y.valueType
            && EqualityComparer<T1>.Default.Equals(x.value1, y.value1)
            && EqualityComparer<T2>.Default.Equals(x.value2, y.value2)
            && EqualityComparer<T3>.Default.Equals(x.value3, y.value3)
            && EqualityComparer<T4>.Default.Equals(x.value4, y.value4)
            && EqualityComparer<T5>.Default.Equals(x.value5, y.value5)
            && EqualityComparer<T6>.Default.Equals(x.value6, y.value6);

        public static bool operator !=(Either<T1, T2, T3, T4, T5, T6> x, Either<T1, T2, T3, T4, T5, T6> y) => !(x == y);
        public override bool Equals(object obj) => obj is Either<T1, T2, T3, T4, T5, T6> e && this == e;

        public override int GetHashCode()
        {
            unchecked
            {
                int hashCode = this.valueType;
                hashCode = (hashCode * 397) ^ EqualityComparer<T1>.Default.GetHashCode(this.value1);
                hashCode = (hashCode * 397) ^ EqualityComparer<T2>.Default.GetHashCode(this.value2);
                hashCode = (hashCode * 397) ^ EqualityComparer<T3>.Default.GetHashCode(this.value3);
                hashCode = (hashCode * 397) ^ EqualityComparer<T4>.Default.GetHashCode(this.value4);
                hashCode = (hashCode * 397) ^ EqualityComparer<T5>.Default.GetHashCode(this.value5);
                hashCode = (hashCode * 397) ^ EqualityComparer<T6>.Default.GetHashCode(this.value6);
                return hashCode;
            }
        }

        public override string ToString() => this.Match(
            v1 => $"First: {v1}",
            v2 => $"Second: {v2}",
            v3 => $"Third: {v3}",
            v4 => $"Fourth: {v4}",
            v5 => $"Fifth: {v5}",
            v6 => $"Sixth: {v6}");
    }

    public struct Either<T1, T2, T3, T4, T5, T6, T7> : IEitherOfSeven
    {
        private readonly int valueType;
        private readonly T1 value1;
        private readonly T2 value2;
        private readonly T3 value3;
        private readonly T4 value4;
        private readonly T5 value5;
        private readonly T6 value6;
        private readonly T7 value7;

        internal Either(int valueType, T1 value1, T2 value2, T3 value3, T4 value4, T5 value5, T6 value6, T7 value7)
        {
            this.valueType = valueType;
            this.value1 = value1;
            this.value2 = value2;
            this.value3 = value3;
            this.value4 = value4;
            this.value5 = value5;
            this.value6 = value6;
            this.value7 = value7;
        }

        #region Type Constructors

        public static Either<T1, T2, T3, T4, T5, T6, T7> First(T1 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(
            0,
            value,
            default(T2),
            default(T3),
            default(T4),
            default(T5),
            default(T6),
            default(T7));

        public static Either<T1, T2, T3, T4, T5, T6, T7> Second(T2 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(
            1,
            default(T1),
            value,
            default(T3),
            default(T4),
            default(T5),
            default(T6),
            default(T7));

        public static Either<T1, T2, T3, T4, T5, T6, T7> Third(T3 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(
            2,
            default(T1),
            default(T2),
            value,
            default(T4),
            default(T5),
            default(T6),
            default(T7));

        public static Either<T1, T2, T3, T4, T5, T6, T7> Fourth(T4 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(
            3,
            default(T1),
            default(T2),
            default(T3),
            value,
            default(T5),
            default(T6),
            default(T7));

        public static Either<T1, T2, T3, T4, T5, T6, T7> Fifth(T5 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(
            4,
            default(T1),
            default(T2),
            default(T3),
            default(T4),
            value,
            default(T6),
            default(T7));

        public static Either<T1, T2, T3, T4, T5, T6, T7> Sixth(T6 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(
            5,
            default(T1),
            default(T2),
            default(T3),
            default(T4),
            default(T5),
            value,
            default(T7));

        public static Either<T1, T2, T3, T4, T5, T6, T7> Seventh(T7 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(
            6,
            default(T1),
            default(T2),
            default(T3),
            default(T4),
            default(T5),
            default(T6),
            value);

        #endregion

        #region Base Functionality

        T IEitherOfSeven.Match<T>(
            Func<object, T> onFirst,
            Func<object, T> onSecond,
            Func<object, T> onThird,
            Func<object, T> onFourth,
            Func<object, T> onFifth,
            Func<object, T> onSixth,
            Func<object, T> onSeventh) =>
            this.Match(
                v => onFirst(v),
                v => onSecond(v),
                v => onThird(v),
                v => onFourth(v),
                v => onFifth(v),
                v => onSixth(v),
                v => onSeventh(v));

        object IEither.GetValueAsObject() => Either.GetValueAs<object>().From(this);

        public T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, T> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, T> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, T> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<T6, T> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<T7, T> onSeventh) =>
            this.valueType == 0
                ? onFirst(this.value1)
                : (this.valueType == 1
                    ? onSecond(this.value2)
                    : (this.valueType == 2
                        ? onThird(this.value3)
                        : (this.valueType == 3
                            ? onFourth(this.value4)
                            : (this.valueType == 4
                                ? onFifth(this.value5)
                                : (this.valueType == 5 ? onSixth(this.value6) : onSeventh(this.value7))))));

        #endregion

        #region Helper Methods

        public void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<T1> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<T2> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<T3> onThird,
            [JetBrains.Annotations.InstantHandle] Action<T4> onFourth,
            [JetBrains.Annotations.InstantHandle] Action<T5> onFifth,
            [JetBrains.Annotations.InstantHandle] Action<T6> onSixth,
            [JetBrains.Annotations.InstantHandle] Action<T7> onSeventh) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Match(
                onFirst.ToFunc(),
                onSecond.ToFunc(),
                onThird.ToFunc(),
                onFourth.ToFunc(),
                onFifth.ToFunc(),
                onSixth.ToFunc(),
                onSeventh.ToFunc());

        public Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task<T>> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, Task<T>> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, Task<T>> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<T6, Task<T>> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<T7, Task<T>> onSeventh) =>
            this.Match(onFirst, onSecond, onThird, onFourth, onFifth, onSixth, onSeventh);

        public Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, Task> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, Task> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<T6, Task> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<T7, Task> onSeventh) =>
            this.MatchAsync(
                onFirst.ToAsyncFunc(),
                onSecond.ToAsyncFunc(),
                onThird.ToAsyncFunc(),
                onFourth.ToAsyncFunc(),
                onFifth.ToAsyncFunc(),
                onSixth.ToAsyncFunc(),
                onSeventh.ToAsyncFunc());

        public Either<T, T2, T3, T4, T5, T6, T7> MapFirst<T>([JetBrains.Annotations.InstantHandle] Func<T1, T> f) =>
            this.Match(
                v1 => Either<T, T2, T3, T4, T5, T6, T7>.First(f(v1)),
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7));

        public Either<T1, T, T3, T4, T5, T6, T7> MapSecond<T>([JetBrains.Annotations.InstantHandle] Func<T2, T> f) =>
            this.Match(
                Either<T1, T, T3, T4, T5, T6, T7>.First,
                v2 => Either.Second(f(v2)),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7));

        public Either<T1, T2, T, T4, T5, T6, T7> MapThird<T>([JetBrains.Annotations.InstantHandle] Func<T3, T> f) =>
            this.Match(
                Either<T1, T2, T, T4, T5, T6, T7>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(f(v3)),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7));

        public Either<T1, T2, T3, T, T5, T6, T7> MapFourth<T>([JetBrains.Annotations.InstantHandle] Func<T4, T> f) =>
            this.Match(
                Either<T1, T2, T3, T, T5, T6, T7>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(f(v4)),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7));

        public Either<T1, T2, T3, T4, T, T6, T7> MapFifth<T>([JetBrains.Annotations.InstantHandle] Func<T5, T> f) =>
            this.Match(
                Either<T1, T2, T3, T4, T, T6, T7>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(f(v5)),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7));

        public Either<T1, T2, T3, T4, T5, T, T7> MapSixth<T>([JetBrains.Annotations.InstantHandle] Func<T6, T> f) =>
            this.Match(
                Either<T1, T2, T3, T4, T5, T, T7>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(f(v6)),
                v7 => Either.Seventh(v7));

        public Either<T1, T2, T3, T4, T5, T6, T> MapSeventh<T>([JetBrains.Annotations.InstantHandle] Func<T7, T> f) =>
            this.Match(
                Either<T1, T2, T3, T4, T5, T6, T>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(f(v7)));

        [JetBrains.Annotations.Pure]
        public Maybe<T1> TryGetFirst() =>
            this.Match(
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T2> TryGetSecond() =>
            this.Match(
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T3> TryGetThird() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T4> TryGetFourth() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T5> TryGetFifth() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T6> TryGetSixth() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T7> TryGetSeventh() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some);

        [JetBrains.Annotations.Pure]
        public bool IsFirst() =>
            this.Match(_ => true, _ => false, _ => false, _ => false, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSecond() =>
            this.Match(_ => false, _ => true, _ => false, _ => false, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsThird() =>
            this.Match(_ => false, _ => false, _ => true, _ => false, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsFourth() =>
            this.Match(_ => false, _ => false, _ => false, _ => true, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsFifth() =>
            this.Match(_ => false, _ => false, _ => false, _ => false, _ => true, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSixth() =>
            this.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => true, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSeventh() =>
            this.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => false, _ => true);

        void IEitherOfSeven.MatchVoid(
            Action<object> onFirst,
            Action<object> onSecond,
            Action<object> onThird,
            Action<object> onFourth,
            Action<object> onFifth,
            Action<object> onSixth,
            Action<object> onSeventh) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Upcast<IEitherOfSeven>()
                .Match(
                    onFirst.ToFunc(),
                    onSecond.ToFunc(),
                    onThird.ToFunc(),
                    onFourth.ToFunc(),
                    onFifth.ToFunc(),
                    onSixth.ToFunc(),
                    onSeventh.ToFunc());

        Task<T> IEitherOfSeven.MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird,
            Func<object, Task<T>> onFourth,
            Func<object, Task<T>> onFifth,
            Func<object, Task<T>> onSixth,
            Func<object, Task<T>> onSeventh) =>
            this.Upcast<IEitherOfSeven>().Match(onFirst, onSecond, onThird, onFourth, onFifth, onSixth, onSeventh);

        Task IEitherOfSeven.MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird,
            Func<object, Task> onFourth,
            Func<object, Task> onFifth,
            Func<object, Task> onSixth,
            Func<object, Task> onSeventh) =>
            this.Upcast<IEitherOfSeven>()
                .MatchAsync(
                    onFirst.ToAsyncFunc(),
                    onSecond.ToAsyncFunc(),
                    onThird.ToAsyncFunc(),
                    onFourth.ToAsyncFunc(),
                    onFifth.ToAsyncFunc(),
                    onSixth.ToAsyncFunc(),
                    onSeventh.ToAsyncFunc());

        #endregion

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherFirst<T1> value) =>
            First(value == null ? default(T1) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherSecond<T2> value) =>
            Second(value == null ? default(T2) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherThird<T3> value) =>
            Third(value == null ? default(T3) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherFourth<T4> value) =>
            Fourth(value == null ? default(T4) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherFifth<T5> value) =>
            Fifth(value == null ? default(T5) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherSixth<T6> value) =>
            Sixth(value == null ? default(T6) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherSeventh<T7> value) =>
            Seventh(value == null ? default(T7) : value.Value);

        public static bool operator ==(Either<T1, T2, T3, T4, T5, T6, T7> x, Either<T1, T2, T3, T4, T5, T6, T7> y) =>
            x.valueType == y.valueType
            && EqualityComparer<T1>.Default.Equals(x.value1, y.value1)
            && EqualityComparer<T2>.Default.Equals(x.value2, y.value2)
            && EqualityComparer<T3>.Default.Equals(x.value3, y.value3)
            && EqualityComparer<T4>.Default.Equals(x.value4, y.value4)
            && EqualityComparer<T5>.Default.Equals(x.value5, y.value5)
            && EqualityComparer<T6>.Default.Equals(x.value6, y.value6)
            && EqualityComparer<T7>.Default.Equals(x.value7, y.value7);

        public static bool operator !=(Either<T1, T2, T3, T4, T5, T6, T7> x, Either<T1, T2, T3, T4, T5, T6, T7> y) =>
            !(x == y);

        public override bool Equals(object obj) => obj is Either<T1, T2, T3, T4, T5, T6, T7> e && this == e;

        public override int GetHashCode()
        {
            unchecked
            {
                int hashCode = this.valueType;
                hashCode = (hashCode * 397) ^ EqualityComparer<T1>.Default.GetHashCode(this.value1);
                hashCode = (hashCode * 397) ^ EqualityComparer<T2>.Default.GetHashCode(this.value2);
                hashCode = (hashCode * 397) ^ EqualityComparer<T3>.Default.GetHashCode(this.value3);
                hashCode = (hashCode * 397) ^ EqualityComparer<T4>.Default.GetHashCode(this.value4);
                hashCode = (hashCode * 397) ^ EqualityComparer<T5>.Default.GetHashCode(this.value5);
                hashCode = (hashCode * 397) ^ EqualityComparer<T6>.Default.GetHashCode(this.value6);
                hashCode = (hashCode * 397) ^ EqualityComparer<T7>.Default.GetHashCode(this.value7);
                return hashCode;
            }
        }

        public override string ToString() => this.Match(
            v1 => $"First: {v1}",
            v2 => $"Second: {v2}",
            v3 => $"Third: {v3}",
            v4 => $"Fourth: {v4}",
            v5 => $"Fifth: {v5}",
            v6 => $"Sixth: {v6}",
            v7 => $"Seventh: {v7}");
    }

    public struct Either<T1, T2, T3, T4, T5, T6, T7, T8> : IEitherOfEight
    {
        private readonly int valueType;
        private readonly T1 value1;
        private readonly T2 value2;
        private readonly T3 value3;
        private readonly T4 value4;
        private readonly T5 value5;
        private readonly T6 value6;
        private readonly T7 value7;
        private readonly T8 value8;

        internal Either(
            int valueType,
            T1 value1,
            T2 value2,
            T3 value3,
            T4 value4,
            T5 value5,
            T6 value6,
            T7 value7,
            T8 value8)
        {
            this.valueType = valueType;
            this.value1 = value1;
            this.value2 = value2;
            this.value3 = value3;
            this.value4 = value4;
            this.value5 = value5;
            this.value6 = value6;
            this.value7 = value7;
            this.value8 = value8;
        }

        #region Type Constructors

        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> First(T1 value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(
                0,
                value,
                default(T2),
                default(T3),
                default(T4),
                default(T5),
                default(T6),
                default(T7),
                default(T8));

        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Second(T2 value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(
                1,
                default(T1),
                value,
                default(T3),
                default(T4),
                default(T5),
                default(T6),
                default(T7),
                default(T8));

        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Third(T3 value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(
                2,
                default(T1),
                default(T2),
                value,
                default(T4),
                default(T5),
                default(T6),
                default(T7),
                default(T8));

        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Fourth(T4 value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(
                3,
                default(T1),
                default(T2),
                default(T3),
                value,
                default(T5),
                default(T6),
                default(T7),
                default(T8));

        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Fifth(T5 value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(
                4,
                default(T1),
                default(T2),
                default(T3),
                default(T4),
                value,
                default(T6),
                default(T7),
                default(T8));

        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Sixth(T6 value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(
                5,
                default(T1),
                default(T2),
                default(T3),
                default(T4),
                default(T5),
                value,
                default(T7),
                default(T8));

        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Seventh(T7 value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(
                6,
                default(T1),
                default(T2),
                default(T3),
                default(T4),
                default(T5),
                default(T6),
                value,
                default(T8));

        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Eighth(T8 value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(
                7,
                default(T1),
                default(T2),
                default(T3),
                default(T4),
                default(T5),
                default(T6),
                default(T7),
                value);

        #endregion

        #region Base Functionality

        T IEitherOfEight.Match<T>(
            Func<object, T> onFirst,
            Func<object, T> onSecond,
            Func<object, T> onThird,
            Func<object, T> onFourth,
            Func<object, T> onFifth,
            Func<object, T> onSixth,
            Func<object, T> onSeventh,
            Func<object, T> onEighth) =>
            this.Match(
                v => onFirst(v),
                v => onSecond(v),
                v => onThird(v),
                v => onFourth(v),
                v => onFifth(v),
                v => onSixth(v),
                v => onSeventh(v),
                v => onEighth(v));

        object IEither.GetValueAsObject() => Either.GetValueAs<object>().From(this);

        public T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, T> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, T> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, T> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<T6, T> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<T7, T> onSeventh,
            [JetBrains.Annotations.InstantHandle] Func<T8, T> onEighth) =>
            this.valueType == 0
                ? onFirst(this.value1)
                : (this.valueType == 1
                    ? onSecond(this.value2)
                    : (this.valueType == 2
                        ? onThird(this.value3)
                        : (this.valueType == 3
                            ? onFourth(this.value4)
                            : (this.valueType == 4
                                ? onFifth(this.value5)
                                : (this.valueType == 5
                                    ? onSixth(this.value6)
                                    : (this.valueType == 6 ? onSeventh(this.value7) : onEighth(this.value8)))))));

        #endregion

        #region Helper Methods

        public void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<T1> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<T2> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<T3> onThird,
            [JetBrains.Annotations.InstantHandle] Action<T4> onFourth,
            [JetBrains.Annotations.InstantHandle] Action<T5> onFifth,
            [JetBrains.Annotations.InstantHandle] Action<T6> onSixth,
            [JetBrains.Annotations.InstantHandle] Action<T7> onSeventh,
            [JetBrains.Annotations.InstantHandle] Action<T8> onEighth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Match(
                onFirst.ToFunc(),
                onSecond.ToFunc(),
                onThird.ToFunc(),
                onFourth.ToFunc(),
                onFifth.ToFunc(),
                onSixth.ToFunc(),
                onSeventh.ToFunc(),
                onEighth.ToFunc());

        public Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task<T>> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, Task<T>> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, Task<T>> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<T6, Task<T>> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<T7, Task<T>> onSeventh,
            [JetBrains.Annotations.InstantHandle] Func<T8, Task<T>> onEighth) =>
            this.Match(onFirst, onSecond, onThird, onFourth, onFifth, onSixth, onSeventh, onEighth);

        public Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<T1, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<T2, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<T3, Task> onThird,
            [JetBrains.Annotations.InstantHandle] Func<T4, Task> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<T5, Task> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<T6, Task> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<T7, Task> onSeventh,
            [JetBrains.Annotations.InstantHandle] Func<T8, Task> onEighth) =>
            this.MatchAsync(
                onFirst.ToAsyncFunc(),
                onSecond.ToAsyncFunc(),
                onThird.ToAsyncFunc(),
                onFourth.ToAsyncFunc(),
                onFifth.ToAsyncFunc(),
                onSixth.ToAsyncFunc(),
                onSeventh.ToAsyncFunc(),
                onEighth.ToAsyncFunc());

        public Either<T, T2, T3, T4, T5, T6, T7, T8> MapFirst<T>([JetBrains.Annotations.InstantHandle] Func<T1, T> f) =>
            this.Match(
                v1 => Either<T, T2, T3, T4, T5, T6, T7, T8>.First(f(v1)),
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7),
                v8 => Either.Eighth(v8));

        public Either<T1, T, T3, T4, T5, T6, T7, T8> MapSecond<T>([JetBrains.Annotations.InstantHandle] Func<T2, T> f) =>
            this.Match(
                Either<T1, T, T3, T4, T5, T6, T7, T8>.First,
                v2 => Either.Second(f(v2)),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7),
                v8 => Either.Eighth(v8));

        public Either<T1, T2, T, T4, T5, T6, T7, T8> MapThird<T>([JetBrains.Annotations.InstantHandle] Func<T3, T> f) =>
            this.Match(
                Either<T1, T2, T, T4, T5, T6, T7, T8>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(f(v3)),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7),
                v8 => Either.Eighth(v8));

        public Either<T1, T2, T3, T, T5, T6, T7, T8> MapFourth<T>([JetBrains.Annotations.InstantHandle] Func<T4, T> f) =>
            this.Match(
                Either<T1, T2, T3, T, T5, T6, T7, T8>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(f(v4)),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7),
                v8 => Either.Eighth(v8));

        public Either<T1, T2, T3, T4, T, T6, T7, T8> MapFifth<T>([JetBrains.Annotations.InstantHandle] Func<T5, T> f) =>
            this.Match(
                Either<T1, T2, T3, T4, T, T6, T7, T8>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(f(v5)),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7),
                v8 => Either.Eighth(v8));

        public Either<T1, T2, T3, T4, T5, T, T7, T8> MapSixth<T>([JetBrains.Annotations.InstantHandle] Func<T6, T> f) =>
            this.Match(
                Either<T1, T2, T3, T4, T5, T, T7, T8>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(f(v6)),
                v7 => Either.Seventh(v7),
                v8 => Either.Eighth(v8));

        public Either<T1, T2, T3, T4, T5, T6, T, T8> MapSeventh<T>([JetBrains.Annotations.InstantHandle] Func<T7, T> f) =>
            this.Match(
                Either<T1, T2, T3, T4, T5, T6, T, T8>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(f(v7)),
                v8 => Either.Eighth(v8));

        public Either<T1, T2, T3, T4, T5, T6, T7, T> MapEighth<T>([JetBrains.Annotations.InstantHandle] Func<T8, T> f) =>
            this.Match(
                Either<T1, T2, T3, T4, T5, T6, T7, T>.First,
                v2 => Either.Second(v2),
                v3 => Either.Third(v3),
                v4 => Either.Fourth(v4),
                v5 => Either.Fifth(v5),
                v6 => Either.Sixth(v6),
                v7 => Either.Seventh(v7),
                v8 => Either.Eighth(f(v8)));

        [JetBrains.Annotations.Pure]
        public Maybe<T1> TryGetFirst() =>
            this.Match(
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T2> TryGetSecond() =>
            this.Match(
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T3> TryGetThird() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T4> TryGetFourth() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T5> TryGetFifth() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T6> TryGetSixth() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T7> TryGetSeventh() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some,
                _ => Maybe.None);

        [JetBrains.Annotations.Pure]
        public Maybe<T8> TryGetEighth() =>
            this.Match(
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                _ => Maybe.None,
                Maybe.Some);

        [JetBrains.Annotations.Pure]
        public bool IsFirst() =>
            this.Match(_ => true, _ => false, _ => false, _ => false, _ => false, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSecond() =>
            this.Match(_ => false, _ => true, _ => false, _ => false, _ => false, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsThird() =>
            this.Match(_ => false, _ => false, _ => true, _ => false, _ => false, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsFourth() =>
            this.Match(_ => false, _ => false, _ => false, _ => true, _ => false, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsFifth() =>
            this.Match(_ => false, _ => false, _ => false, _ => false, _ => true, _ => false, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSixth() =>
            this.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => true, _ => false, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsSeventh() =>
            this.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => false, _ => true, _ => false);

        [JetBrains.Annotations.Pure]
        public bool IsEighth() =>
            this.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => false, _ => false, _ => true);

        void IEitherOfEight.MatchVoid(
            Action<object> onFirst,
            Action<object> onSecond,
            Action<object> onThird,
            Action<object> onFourth,
            Action<object> onFifth,
            Action<object> onSixth,
            Action<object> onSeventh,
            Action<object> onEighth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            this.Upcast<IEitherOfEight>()
                .Match(
                    onFirst.ToFunc(),
                    onSecond.ToFunc(),
                    onThird.ToFunc(),
                    onFourth.ToFunc(),
                    onFifth.ToFunc(),
                    onSixth.ToFunc(),
                    onSeventh.ToFunc(),
                    onEighth.ToFunc());

        Task<T> IEitherOfEight.MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird,
            Func<object, Task<T>> onFourth,
            Func<object, Task<T>> onFifth,
            Func<object, Task<T>> onSixth,
            Func<object, Task<T>> onSeventh,
            Func<object, Task<T>> onEighth) =>
            this.Upcast<IEitherOfEight>()
                .Match(onFirst, onSecond, onThird, onFourth, onFifth, onSixth, onSeventh, onEighth);

        Task IEitherOfEight.MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird,
            Func<object, Task> onFourth,
            Func<object, Task> onFifth,
            Func<object, Task> onSixth,
            Func<object, Task> onSeventh,
            Func<object, Task> onEighth) =>
            this.Upcast<IEitherOfEight>()
                .MatchAsync(
                    onFirst.ToAsyncFunc(),
                    onSecond.ToAsyncFunc(),
                    onThird.ToAsyncFunc(),
                    onFourth.ToAsyncFunc(),
                    onFifth.ToAsyncFunc(),
                    onSixth.ToAsyncFunc(),
                    onSeventh.ToAsyncFunc(),
                    onEighth.ToAsyncFunc());

        #endregion

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherFirst<T1> value) =>
            First(value == null ? default(T1) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherSecond<T2> value) =>
            Second(value == null ? default(T2) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherThird<T3> value) =>
            Third(value == null ? default(T3) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherFourth<T4> value) =>
            Fourth(value == null ? default(T4) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherFifth<T5> value) =>
            Fifth(value == null ? default(T5) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherSixth<T6> value) =>
            Sixth(value == null ? default(T6) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherSeventh<T7> value) =>
            Seventh(value == null ? default(T7) : value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherEighth<T8> value) =>
            Eighth(value == null ? default(T8) : value.Value);

        public static bool operator ==(
            Either<T1, T2, T3, T4, T5, T6, T7, T8> x,
            Either<T1, T2, T3, T4, T5, T6, T7, T8> y) =>
            x.valueType == y.valueType
            && EqualityComparer<T1>.Default.Equals(x.value1, y.value1)
            && EqualityComparer<T2>.Default.Equals(x.value2, y.value2)
            && EqualityComparer<T3>.Default.Equals(x.value3, y.value3)
            && EqualityComparer<T4>.Default.Equals(x.value4, y.value4)
            && EqualityComparer<T5>.Default.Equals(x.value5, y.value5)
            && EqualityComparer<T6>.Default.Equals(x.value6, y.value6)
            && EqualityComparer<T7>.Default.Equals(x.value7, y.value7)
            && EqualityComparer<T8>.Default.Equals(x.value8, y.value8);

        public static bool operator !=(
            Either<T1, T2, T3, T4, T5, T6, T7, T8> x,
            Either<T1, T2, T3, T4, T5, T6, T7, T8> y) => !(x == y);

        public override bool Equals(object obj) => obj is Either<T1, T2, T3, T4, T5, T6, T7, T8> e && this == e;

        public override int GetHashCode()
        {
            unchecked
            {
                int hashCode = this.valueType;
                hashCode = (hashCode * 397) ^ EqualityComparer<T1>.Default.GetHashCode(this.value1);
                hashCode = (hashCode * 397) ^ EqualityComparer<T2>.Default.GetHashCode(this.value2);
                hashCode = (hashCode * 397) ^ EqualityComparer<T3>.Default.GetHashCode(this.value3);
                hashCode = (hashCode * 397) ^ EqualityComparer<T4>.Default.GetHashCode(this.value4);
                hashCode = (hashCode * 397) ^ EqualityComparer<T5>.Default.GetHashCode(this.value5);
                hashCode = (hashCode * 397) ^ EqualityComparer<T6>.Default.GetHashCode(this.value6);
                hashCode = (hashCode * 397) ^ EqualityComparer<T7>.Default.GetHashCode(this.value7);
                hashCode = (hashCode * 397) ^ EqualityComparer<T8>.Default.GetHashCode(this.value8);
                return hashCode;
            }
        }

        public override string ToString() => this.Match(
            v1 => $"First: {v1}",
            v2 => $"Second: {v2}",
            v3 => $"Third: {v3}",
            v4 => $"Fourth: {v4}",
            v5 => $"Fifth: {v5}",
            v6 => $"Sixth: {v6}",
            v7 => $"Seventh: {v7}",
            v8 => $"Eighth: {v8}");
    }
}