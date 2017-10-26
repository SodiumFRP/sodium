using System;

namespace Sodium
{
    public static class Either
    {
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

        public static EitherFirst<T> First<T>(T value) => new EitherFirst<T>(value);
        public static EitherSecond<T> Second<T>(T value) => new EitherSecond<T>(value);
        public static EitherThird<T> Third<T>(T value) => new EitherThird<T>(value);
        public static EitherFourth<T> Fourth<T>(T value) => new EitherFourth<T>(value);
        public static EitherFifth<T> Fifth<T>(T value) => new EitherFifth<T>(value);
        public static EitherSixth<T> Sixth<T>(T value) => new EitherSixth<T>(value);
        public static EitherSeventh<T> Seventh<T>(T value) => new EitherSeventh<T>(value);
        public static EitherEighth<T> Eighth<T>(T value) => new EitherEighth<T>(value);

        public static Either<T1, T2> First<T1, T2>(T1 value) => First(value);
        public static Either<T1, T2> Second<T1, T2>(T2 value) => Second(value);

        public static Either<T1, T2, T3> First<T1, T2, T3>(T1 value) => First(value);
        public static Either<T1, T2, T3> Second<T1, T2, T3>(T2 value) => Second(value);
        public static Either<T1, T2, T3> Third<T1, T2, T3>(T3 value) => Third(value);

        public static Either<T1, T2, T3, T4> First<T1, T2, T3, T4>(T1 value) => First(value);
        public static Either<T1, T2, T3, T4> Second<T1, T2, T3, T4>(T2 value) => Second(value);
        public static Either<T1, T2, T3, T4> Third<T1, T2, T3, T4>(T3 value) => Third(value);
        public static Either<T1, T2, T3, T4> Fourth<T1, T2, T3, T4>(T4 value) => Fourth(value);

        public static Either<T1, T2, T3, T4, T5> First<T1, T2, T3, T4, T5>(T1 value) => First(value);
        public static Either<T1, T2, T3, T4, T5> Second<T1, T2, T3, T4, T5>(T2 value) => Second(value);
        public static Either<T1, T2, T3, T4, T5> Third<T1, T2, T3, T4, T5>(T3 value) => Third(value);
        public static Either<T1, T2, T3, T4, T5> Fourth<T1, T2, T3, T4, T5>(T4 value) => Fourth(value);
        public static Either<T1, T2, T3, T4, T5> Fifth<T1, T2, T3, T4, T5>(T5 value) => Fifth(value);

        public static Either<T1, T2, T3, T4, T5, T6> First<T1, T2, T3, T4, T5, T6>(T1 value) => First(value);
        public static Either<T1, T2, T3, T4, T5, T6> Second<T1, T2, T3, T4, T5, T6>(T2 value) => Second(value);
        public static Either<T1, T2, T3, T4, T5, T6> Third<T1, T2, T3, T4, T5, T6>(T3 value) => Third(value);
        public static Either<T1, T2, T3, T4, T5, T6> Fourth<T1, T2, T3, T4, T5, T6>(T4 value) => Fourth(value);
        public static Either<T1, T2, T3, T4, T5, T6> Fifth<T1, T2, T3, T4, T5, T6>(T5 value) => Fifth(value);
        public static Either<T1, T2, T3, T4, T5, T6> Sixth<T1, T2, T3, T4, T5, T6>(T6 value) => Sixth(value);

        public static Either<T1, T2, T3, T4, T5, T6, T7> First<T1, T2, T3, T4, T5, T6, T7>(T1 value) => First(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7> Second<T1, T2, T3, T4, T5, T6, T7>(T2 value) => Second(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7> Third<T1, T2, T3, T4, T5, T6, T7>(T3 value) => Third(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7> Fourth<T1, T2, T3, T4, T5, T6, T7>(T4 value) => Fourth(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7> Fifth<T1, T2, T3, T4, T5, T6, T7>(T5 value) => Fifth(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7> Sixth<T1, T2, T3, T4, T5, T6, T7>(T6 value) => Sixth(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7> Seventh<T1, T2, T3, T4, T5, T6, T7>(T7 value) => Seventh(value);

        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> First<T1, T2, T3, T4, T5, T6, T7, T8>(T1 value) => First(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Second<T1, T2, T3, T4, T5, T6, T7, T8>(T2 value) => Second(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Third<T1, T2, T3, T4, T5, T6, T7, T8>(T3 value) => Third(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Fourth<T1, T2, T3, T4, T5, T6, T7, T8>(T4 value) => Fourth(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Fifth<T1, T2, T3, T4, T5, T6, T7, T8>(T5 value) => Fifth(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Sixth<T1, T2, T3, T4, T5, T6, T7, T8>(T6 value) => Sixth(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Seventh<T1, T2, T3, T4, T5, T6, T7, T8>(T7 value) => Seventh(value);
        public static Either<T1, T2, T3, T4, T5, T6, T7, T8> Eighth<T1, T2, T3, T4, T5, T6, T7, T8>(T8 value) => Eighth(value);
    }

    public struct Either<T1, T2>
    {
        private readonly int valueType;
        private readonly object value;

        private Either(int valueType, object value)
        {
            this.valueType = valueType;
            this.value = value;
        }

        public void Switch(Action<T1> onFirst, Action<T2> onSecond)
        {
            if (this.valueType == 1)
            {
                onFirst((T1)this.value);
            }
            else
            {
                onSecond((T2)this.value);
            }
        }

        public T Switch<T>(Func<T1, T> onFirst, Func<T2, T> onSecond) =>
            this.valueType == 1 ? onFirst((T1)this.value) : onSecond((T2)this.value);

        public static implicit operator Either<T1, T2>(Either.EitherFirst<T1> value) =>
            new Either<T1, T2>(1, value.Value);

        public static implicit operator Either<T1, T2>(Either.EitherSecond<T2> value) =>
            new Either<T1, T2>(2, value.Value);
    }

    public struct Either<T1, T2, T3>
    {
        private readonly int valueType;
        private readonly object value;

        private Either(int valueType, object value)
        {
            this.valueType = valueType;
            this.value = value;
        }

        public void Switch(Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird)
        {
            if (this.valueType == 1)
            {
                onFirst((T1)this.value);
            }
            else if (this.valueType == 2)
            {
                onSecond((T2)this.value);
            }
            else
            {
                onThird((T3)this.value);
            }
        }

        public T Switch<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird) =>
            this.valueType == 1
                ? onFirst((T1)this.value)
                : (this.valueType == 2 ? onSecond((T2)this.value) : onThird((T3)this.value));

        public static implicit operator Either<T1, T2, T3>(Either.EitherFirst<T1> value) =>
            new Either<T1, T2, T3>(1, value.Value);

        public static implicit operator Either<T1, T2, T3>(Either.EitherSecond<T2> value) =>
            new Either<T1, T2, T3>(2, value.Value);

        public static implicit operator Either<T1, T2, T3>(Either.EitherThird<T3> value) =>
            new Either<T1, T2, T3>(3, value.Value);
    }

    public struct Either<T1, T2, T3, T4>
    {
        private readonly int valueType;
        private readonly object value;

        private Either(int valueType, object value)
        {
            this.valueType = valueType;
            this.value = value;
        }

        public void Switch(Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird, Action<T4> onFourth)
        {
            if (this.valueType == 1)
            {
                onFirst((T1)this.value);
            }
            else if (this.valueType == 2)
            {
                onSecond((T2)this.value);
            }
            else if (this.valueType == 3)
            {
                onThird((T3)this.value);
            }
            else
            {
                onFourth((T4)this.value);
            }
        }

        public T Switch<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird, Func<T4, T> onFourth) =>
            this.valueType == 1
                ? onFirst((T1)this.value)
                : (this.valueType == 2
                    ? onSecond((T2)this.value)
                    : (this.valueType == 3 ? onThird((T3)this.value) : onFourth((T4)this.value)));

        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherFirst<T1> value) =>
            new Either<T1, T2, T3, T4>(1, value.Value);

        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherSecond<T2> value) =>
            new Either<T1, T2, T3, T4>(2, value.Value);

        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherThird<T3> value) =>
            new Either<T1, T2, T3, T4>(3, value.Value);

        public static implicit operator Either<T1, T2, T3, T4>(Either.EitherFourth<T4> value) =>
            new Either<T1, T2, T3, T4>(4, value.Value);
    }

    public struct Either<T1, T2, T3, T4, T5>
    {
        private readonly int valueType;
        private readonly object value;

        private Either(int valueType, object value)
        {
            this.valueType = valueType;
            this.value = value;
        }

        public void Switch(Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird, Action<T4> onFourth, Action<T5> onFifth)
        {
            if (this.valueType == 1)
            {
                onFirst((T1)this.value);
            }
            else if (this.valueType == 2)
            {
                onSecond((T2)this.value);
            }
            else if (this.valueType == 3)
            {
                onThird((T3)this.value);
            }
            else if (this.valueType == 4)
            {
                onFourth((T4)this.value);
            }
            else
            {
                onFifth((T5)this.value);
            }
        }

        public T Switch<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird, Func<T4, T> onFourth, Func<T5, T> onFifth) =>
            this.valueType == 1
                ? onFirst((T1)this.value)
                : (this.valueType == 2
                    ? onSecond((T2)this.value)
                    : (this.valueType == 3
                        ? onThird((T3)this.value)
                        : (this.valueType == 4 ? onFourth((T4)this.value) : onFifth((T5)this.value))));

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherFirst<T1> value) =>
            new Either<T1, T2, T3, T4, T5>(1, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherSecond<T2> value) =>
            new Either<T1, T2, T3, T4, T5>(2, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherThird<T3> value) =>
            new Either<T1, T2, T3, T4, T5>(3, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherFourth<T4> value) =>
            new Either<T1, T2, T3, T4, T5>(4, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5>(Either.EitherFifth<T5> value) =>
            new Either<T1, T2, T3, T4, T5>(5, value.Value);
    }

    public struct Either<T1, T2, T3, T4, T5, T6>
    {
        private readonly int valueType;
        private readonly object value;

        internal Either(int valueType, object value)
        {
            this.valueType = valueType;
            this.value = value;
        }

        public void Switch(Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird, Action<T4> onFourth, Action<T5> onFifth, Action<T6> onSixth)
        {
            if (this.valueType == 1)
            {
                onFirst((T1)this.value);
            }
            else if (this.valueType == 2)
            {
                onSecond((T2)this.value);
            }
            else if (this.valueType == 3)
            {
                onThird((T3)this.value);
            }
            else if (this.valueType == 4)
            {
                onFourth((T4)this.value);
            }
            else if (this.valueType == 5)
            {
                onFifth((T5)this.value);
            }
            else
            {
                onSixth((T6)this.value);
            }
        }

        public T Switch<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird, Func<T4, T> onFourth, Func<T5, T> onFifth, Func<T6, T> onSixth) =>
            this.valueType == 1
                ? onFirst((T1)this.value)
                : (this.valueType == 2
                    ? onSecond((T2)this.value)
                    : (this.valueType == 3
                        ? onThird((T3)this.value)
                        : (this.valueType == 4
                            ? onFourth((T4)this.value)
                            : (this.valueType == 5 ? onFifth((T5)this.value) : onSixth((T6)this.value)))));

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherFirst<T1> value) =>
            new Either<T1, T2, T3, T4, T5, T6>(1, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherSecond<T2> value) =>
            new Either<T1, T2, T3, T4, T5, T6>(2, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherThird<T3> value) =>
            new Either<T1, T2, T3, T4, T5, T6>(3, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherFourth<T4> value) =>
            new Either<T1, T2, T3, T4, T5, T6>(4, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherFifth<T5> value) =>
            new Either<T1, T2, T3, T4, T5, T6>(5, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6>(Either.EitherSixth<T6> value) =>
            new Either<T1, T2, T3, T4, T5, T6>(6, value.Value);
    }

    public struct Either<T1, T2, T3, T4, T5, T6, T7>
    {
        private readonly int valueType;
        private readonly object value;

        internal Either(int valueType, object value)
        {
            this.valueType = valueType;
            this.value = value;
        }

        public void Switch(Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird, Action<T4> onFourth, Action<T5> onFifth, Action<T6> onSixth, Action<T7> onSeventh)
        {
            if (this.valueType == 1)
            {
                onFirst((T1)this.value);
            }
            else if (this.valueType == 2)
            {
                onSecond((T2)this.value);
            }
            else if (this.valueType == 3)
            {
                onThird((T3)this.value);
            }
            else if (this.valueType == 4)
            {
                onFourth((T4)this.value);
            }
            else if (this.valueType == 5)
            {
                onFifth((T5)this.value);
            }
            else if (this.valueType == 6)
            {
                onSixth((T6)this.value);
            }
            else
            {
                onSeventh((T7)this.value);
            }
        }

        public T Switch<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird, Func<T4, T> onFourth, Func<T5, T> onFifth, Func<T6, T> onSixth, Func<T7, T> onSeventh) =>
            this.valueType == 1
                ? onFirst((T1)this.value)
                : (this.valueType == 2
                    ? onSecond((T2)this.value)
                    : (this.valueType == 3
                        ? onThird((T3)this.value)
                        : (this.valueType == 4
                            ? onFourth((T4)this.value)
                            : (this.valueType == 5
                                ? onFifth((T5)this.value)
                                : (this.valueType == 6 ? onSixth((T6)this.value) : onSeventh((T7)this.value))))));

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherFirst<T1> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7>(1, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherSecond<T2> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7>(2, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherThird<T3> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7>(3, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherFourth<T4> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7>(4, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherFifth<T5> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7>(5, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherSixth<T6> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7>(6, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7>(Either.EitherSeventh<T7> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7>(7, value.Value);
    }

    public struct Either<T1, T2, T3, T4, T5, T6, T7, T8>
    {
        private readonly int valueType;
        private readonly object value;

        internal Either(int valueType, object value)
        {
            this.valueType = valueType;
            this.value = value;
        }

        public void Switch(Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird, Action<T4> onFourth, Action<T5> onFifth, Action<T6> onSixth, Action<T7> onSeventh, Action<T8> onEighth)
        {
            if (this.valueType == 1)
            {
                onFirst((T1)this.value);
            }
            else if (this.valueType == 2)
            {
                onSecond((T2)this.value);
            }
            else if (this.valueType == 3)
            {
                onThird((T3)this.value);
            }
            else if (this.valueType == 4)
            {
                onFourth((T4)this.value);
            }
            else if (this.valueType == 5)
            {
                onFifth((T5)this.value);
            }
            else if (this.valueType == 6)
            {
                onSixth((T6)this.value);
            }
            else if (this.valueType == 7)
            {
                onSeventh((T7)this.value);
            }
            else
            {
                onEighth((T8)this.value);
            }
        }

        public T Switch<T>(Func<T1, T> onFirst, Func<T2, T> onSecond, Func<T3, T> onThird, Func<T4, T> onFourth, Func<T5, T> onFifth, Func<T6, T> onSixth, Func<T7, T> onSeventh, Func<T8, T> onEighth) =>
            this.valueType == 1
                ? onFirst((T1)this.value)
                : (this.valueType == 2
                    ? onSecond((T2)this.value)
                    : (this.valueType == 3
                        ? onThird((T3)this.value)
                        : (this.valueType == 4
                            ? onFourth((T4)this.value)
                            : (this.valueType == 5
                                ? onFifth((T5)this.value)
                                : (this.valueType == 6
                                    ? onSixth((T6)this.value)
                                    : (this.valueType == 7 ? onSeventh((T7)this.value) : onEighth((T8)this.value)))))));

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherFirst<T1> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(1, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherSecond<T2> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(2, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherThird<T3> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(3, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherFourth<T4> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(4, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherFifth<T5> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(5, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherSixth<T6> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(6, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherSeventh<T7> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(7, value.Value);

        public static implicit operator Either<T1, T2, T3, T4, T5, T6, T7, T8>(Either.EitherEighth<T8> value) =>
            new Either<T1, T2, T3, T4, T5, T6, T7, T8>(8, value.Value);
    }
}