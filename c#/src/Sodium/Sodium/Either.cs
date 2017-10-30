using System;
using JetBrains.Annotations;

namespace Sodium
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
    }

    public struct Either<T1, T2> : IEither
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

        public static Either<T1, T2> First(T1 value) => new Either<T1, T2>(0, value, default(T2));
        public static Either<T1, T2> Second(T2 value) => new Either<T1, T2>(1, default(T1), value);

        object IEither.GetValueAsObject() => this.Match(v1 => (object)v1, v2 => v2);

        [Pure]
        public T Match<T>(Func<T1, T> onFirst, Func<T2, T> onSecond) =>
            this.valueType == 0 ? onFirst(this.value1) : onSecond(this.value2);

        public static implicit operator Either<T1, T2>(Either.EitherFirst<T1> value) => First(value == null ? default(T1) : value.Value);
        public static implicit operator Either<T1, T2>(Either.EitherSecond<T2> value) => Second(value == null ? default(T2) : value.Value);

        public override string ToString() => this.Match(v1 => $"First: {v1}", v2 => $"Second: {v2}");
    }

    public struct Either<T1, T2, T3> : IEither
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

        public static Either<T1, T2, T3> First(T1 value) => new Either<T1, T2, T3>(0, value, default(T2), default(T3));
        public static Either<T1, T2, T3> Second(T2 value) => new Either<T1, T2, T3>(1, default(T1), value, default(T3));
        public static Either<T1, T2, T3> Third(T3 value) => new Either<T1, T2, T3>(2, default(T1), default(T2), value);

        object IEither.GetValueAsObject() => this.Match(v1 => (object)v1, v2 => v2, v3 => v3);

        [Pure]
        public T Match<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird) =>
            this.valueType == 0
                ? onFirst(this.value1)
                : (this.valueType == 1 ? onSecond(this.value2) : onThird(this.value3));

        public static implicit operator Either<T1, T2, T3>(Either.EitherFirst<T1> value) => First(value == null ? default(T1) : value.Value);
        public static implicit operator Either<T1, T2, T3>(Either.EitherSecond<T2> value) => Second(value == null ? default(T2) : value.Value);
        public static implicit operator Either<T1, T2, T3>(Either.EitherThird<T3> value) => Third(value == null ? default(T3) : value.Value);

        public override string ToString() => this.Match(v1 => $"First: {v1}", v2 => $"Second: {v2}", v3 => $"Third: {v3}");
    }

    public struct Either<T1, T2, T3, T4> : IEither
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

        public static Either<T1, T2, T3, T4> First(T1 value) => new Either<T1, T2, T3, T4>(0, value, default(T2), default(T3), default(T4));
        public static Either<T1, T2, T3, T4> Second(T2 value) => new Either<T1, T2, T3, T4>(1, default(T1), value, default(T3), default(T4));
        public static Either<T1, T2, T3, T4> Third(T3 value) => new Either<T1, T2, T3, T4>(2, default(T1), default(T2), value, default(T4));
        public static Either<T1, T2, T3, T4> Fourth(T4 value) => new Either<T1, T2, T3, T4>(3, default(T1), default(T2), default(T3), value);

        object IEither.GetValueAsObject() => this.Match(v1 => (object)v1, v2 => v2, v3 => v3, v4 => v4);

        [Pure]
        public T Match<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird, Func<T4, T> onFourth) =>
            this.valueType == 0
                ? onFirst(this.value1)
                : (this.valueType == 1
                    ? onSecond(this.value2)
                    : (this.valueType == 2 ? onThird(this.value3) : onFourth(this.value4)));

        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherFirst<T1> value) => First(value == null ? default(T1) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherSecond<T2> value) => Second(value == null ? default(T2) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherThird<T3> value) => Third(value == null ? default(T3) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherFourth<T4> value) => Fourth(value == null ? default(T4) : value.Value);

        public override string ToString() => this.Match(v1 => $"First: {v1}", v2 => $"Second: {v2}", v3 => $"Third: {v3}", v4 => $"Fourth: {v4}");
    }

    public struct Either<T1, T2, T3, T4, T5> : IEither
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

        public static Either<T1, T2, T3, T4, T5> First(T1 value) => new Either<T1, T2, T3, T4, T5>(0, value, default(T2), default(T3), default(T4), default(T5));
        public static Either<T1, T2, T3, T4, T5> Second(T2 value) => new Either<T1, T2, T3, T4, T5>(1, default(T1), value, default(T3), default(T4), default(T5));
        public static Either<T1, T2, T3, T4, T5> Third(T3 value) => new Either<T1, T2, T3, T4, T5>(2, default(T1), default(T2), value, default(T4), default(T5));
        public static Either<T1, T2, T3, T4, T5> Fourth(T4 value) => new Either<T1, T2, T3, T4, T5>(3, default(T1), default(T2), default(T3), value, default(T5));
        public static Either<T1, T2, T3, T4, T5> Fifth(T5 value) => new Either<T1, T2, T3, T4, T5>(4, default(T1), default(T2), default(T3), default(T4), value);

        object IEither.GetValueAsObject() => this.Match(v1 => (object)v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5);

        [Pure]
        public T Match<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird, Func<T4, T> onFourth, Func<T5, T> onFifth) =>
            this.valueType == 0
                ? onFirst(this.value1)
                : (this.valueType == 1
                    ? onSecond(this.value2)
                    : (this.valueType == 2
                        ? onThird(this.value3)
                        : (this.valueType == 3 ? onFourth(this.value4) : onFifth(this.value5))));

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherFirst<T1> value) => First(value == null ? default(T1) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherSecond<T2> value) => Second(value == null ? default(T2) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherThird<T3> value) => Third(value == null ? default(T3) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherFourth<T4> value) => Fourth(value == null ? default(T4) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherFifth<T5> value) => Fifth(value == null ? default(T5) : value.Value);

        public override string ToString() => this.Match(v1 => $"First: {v1}", v2 => $"Second: {v2}", v3 => $"Third: {v3}", v4 => $"Fourth: {v4}", v5 => $"Fifth: {v5}");
    }

    public struct Either<T1, T2, T3, T4, T5, T6> : IEither
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

        public static Either<T1, T2, T3, T4, T5, T6> First(T1 value) => new Either<T1, T2, T3, T4, T5, T6>(0, value, default(T2), default(T3), default(T4), default(T5), default(T6));
        public static Either<T1, T2, T3, T4, T5, T6> Second(T2 value) => new Either<T1, T2, T3, T4, T5, T6>(1, default(T1), value, default(T3), default(T4), default(T5), default(T6));
        public static Either<T1, T2, T3, T4, T5, T6> Third(T3 value) => new Either<T1, T2, T3, T4, T5, T6>(2, default(T1), default(T2), value, default(T4), default(T5), default(T6));
        public static Either<T1, T2, T3, T4, T5, T6> Fourth(T4 value) => new Either<T1, T2, T3, T4, T5, T6>(3, default(T1), default(T2), default(T3), value, default(T5), default(T6));
        public static Either<T1, T2, T3, T4, T5, T6> Fifth(T5 value) => new Either<T1, T2, T3, T4, T5, T6>(4, default(T1), default(T2), default(T3), default(T4), value, default(T6));
        public static Either<T1, T2, T3, T4, T5, T6> Sixth(T6 value) => new Either<T1, T2, T3, T4, T5, T6>(5, default(T1), default(T2), default(T3), default(T4), default(T5), value);

        object IEither.GetValueAsObject() => this.Match(v1 => (object)v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5, v6 => v6);

        [Pure]
        public T Match<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird, Func<T4, T> onFourth, Func<T5, T> onFifth, Func<T6, T> onSixth) =>
            this.valueType == 0
                ? onFirst(this.value1)
                : (this.valueType == 1
                    ? onSecond(this.value2)
                    : (this.valueType == 2
                        ? onThird(this.value3)
                        : (this.valueType == 3
                            ? onFourth(this.value4)
                            : (this.valueType == 4 ? onFifth(this.value5) : onSixth(this.value6)))));

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherFirst<T1> value) => First(value == null ? default(T1) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherSecond<T2> value) => Second(value == null ? default(T2) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherThird<T3> value) => Third(value == null ? default(T3) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherFourth<T4> value) => Fourth(value == null ? default(T4) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherFifth<T5> value) => Fifth(value == null ? default(T5) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherSixth<T6> value) => Sixth(value == null ? default(T6) : value.Value);

        public override string ToString() => this.Match(v1 => $"First: {v1}", v2 => $"Second: {v2}", v3 => $"Third: {v3}", v4 => $"Fourth: {v4}", v5 => $"Fifth: {v5}", v6 => $"Sixth: {v6}");
    }

    public struct Either<T1, T2, T3, T4, T5, T6, T7> : IEither
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

        public static Either<T1, T2, T3, T4, T5, T6, T7> First(T1 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(0, value, default(T2), default(T3), default(T4), default(T5), default(T6), default(T7));
        public static Either<T1, T2, T3, T4, T5, T6, T7> Second(T2 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(1, default(T1), value, default(T3), default(T4), default(T5), default(T6), default(T7));
        public static Either<T1, T2, T3, T4, T5, T6, T7> Third(T3 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(2, default(T1), default(T2), value, default(T4), default(T5), default(T6), default(T7));
        public static Either<T1, T2, T3, T4, T5, T6, T7> Fourth(T4 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(3, default(T1), default(T2), default(T3), value, default(T5), default(T6), default(T7));
        public static Either<T1, T2, T3, T4, T5, T6, T7> Fifth(T5 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(4, default(T1), default(T2), default(T3), default(T4), value, default(T6), default(T7));
        public static Either<T1, T2, T3, T4, T5, T6, T7> Sixth(T6 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(5, default(T1), default(T2), default(T3), default(T4), default(T5), value, default(T7));
        public static Either<T1, T2, T3, T4, T5, T6, T7> Seventh(T7 value) => new Either<T1, T2, T3, T4, T5, T6, T7>(6, default(T1), default(T2), default(T3), default(T4), default(T5), default(T6), value);

        object IEither.GetValueAsObject() => this.Match(v1 => (object)v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5, v6 => v6, v7 => v7);

        [Pure]
        public T Match<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird, Func<T4, T> onFourth, Func<T5, T> onFifth, Func<T6, T> onSixth, Func<T7, T> onSeventh) =>
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

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherFirst<T1> value) => First(value == null ? default(T1) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherSecond<T2> value) => Second(value == null ? default(T2) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherThird<T3> value) => Third(value == null ? default(T3) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherFourth<T4> value) => Fourth(value == null ? default(T4) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherFifth<T5> value) => Fifth(value == null ? default(T5) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherSixth<T6> value) => Sixth(value == null ? default(T6) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherSeventh<T7> value) => Seventh(value == null ? default(T7) : value.Value);

        public override string ToString() => this.Match(v1 => $"First: {v1}", v2 => $"Second: {v2}", v3 => $"Third: {v3}", v4 => $"Fourth: {v4}", v5 => $"Fifth: {v5}", v6 => $"Sixth: {v6}", v7 => $"Seventh: {v7}");
    }

    public struct Either<T1, T2, T3, T4, T5, T6, T7, T8> : IEither
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

        internal Either(int valueType, T1 value1, T2 value2, T3 value3, T4 value4, T5 value5, T6 value6, T7 value7, T8 value8)
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

        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> First(T1 value) => new Either<T1, T2, T3, T4, T5, T6, T7, T8>(0, value, default(T2), default(T3), default(T4), default(T5), default(T6), default(T7), default(T8));
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Second(T2 value) => new Either<T1, T2, T3, T4, T5, T6, T7, T8>(1, default(T1), value, default(T3), default(T4), default(T5), default(T6), default(T7), default(T8));
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Third(T3 value) => new Either<T1, T2, T3, T4, T5, T6, T7, T8>(2, default(T1), default(T2), value, default(T4), default(T5), default(T6), default(T7), default(T8));
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Fourth(T4 value) => new Either<T1, T2, T3, T4, T5, T6, T7, T8>(3, default(T1), default(T2), default(T3), value, default(T5), default(T6), default(T7), default(T8));
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Fifth(T5 value) => new Either<T1, T2, T3, T4, T5, T6, T7, T8>(4, default(T1), default(T2), default(T3), default(T4), value, default(T6), default(T7), default(T8));
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Sixth(T6 value) => new Either<T1, T2, T3, T4, T5, T6, T7, T8>(5, default(T1), default(T2), default(T3), default(T4), default(T5), value, default(T7), default(T8));
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Seventh(T7 value) => new Either<T1, T2, T3, T4, T5, T6, T7, T8>(6, default(T1), default(T2), default(T3), default(T4), default(T5), default(T6), value, default(T8));
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Eighth(T8 value) => new Either<T1, T2, T3, T4, T5, T6, T7, T8>(7, default(T1), default(T2), default(T3), default(T4), default(T5), default(T6), default(T7), value);

        object IEither.GetValueAsObject() => this.Match(v1 => (object)v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5, v6 => v6, v7 => v7, v8 => v8);

        [Pure]
        public T Match<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird, Func<T4, T> onFourth, Func<T5, T> onFifth, Func<T6, T> onSixth, Func<T7, T> onSeventh, Func<T8, T> onEighth) =>
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

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherFirst<T1> value) => First(value == null ? default(T1) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherSecond<T2> value) => Second(value == null ? default(T2) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherThird<T3> value) => Third(value == null ? default(T3) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherFourth<T4> value) => Fourth(value == null ? default(T4) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherFifth<T5> value) => Fifth(value == null ? default(T5) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherSixth<T6> value) => Sixth(value == null ? default(T6) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherSeventh<T7> value) => Seventh(value == null ? default(T7) : value.Value);
        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherEighth<T8> value) => Eighth(value == null ? default(T8) : value.Value);

        public override string ToString() => this.Match(v1 => $"First: {v1}", v2 => $"Second: {v2}", v3 => $"Third: {v3}", v4 => $"Fourth: {v4}", v5 => $"Fifth: {v5}", v6 => $"Sixth: {v6}", v7 => $"Seventh: {v7}", v8 => $"Eighth: {v8}");
    }
}