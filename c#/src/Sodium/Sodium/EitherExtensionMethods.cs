using System;
using System.Threading.Tasks;

namespace Sodium
{
    public static class EitherExtensionMethods
    {
        public static void MatchVoid<T1, T2>(this Either<T1, T2> o, Action<T1> onFirst, Action<T2> onSecond) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            o.Match(onFirst.ToFunc(), onSecond.ToFunc());

        public static void MatchVoid<T1, T2, T3>(this Either<T1, T2, T3> o, Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            o.Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc());

        public static void MatchVoid<T1, T2, T3, T4>(this Either<T1, T2, T3, T4> o, Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird, Action<T4> onFourth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            o.Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc(), onFourth.ToFunc());

        public static void MatchVoid<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> o, Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird, Action<T4> onFourth, Action<T5> onFifth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            o.Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc(), onFourth.ToFunc(), onFifth.ToFunc());

        public static void MatchVoid<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> o, Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird, Action<T4> onFourth, Action<T5> onFifth, Action<T6> onSixth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            o.Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc(), onFourth.ToFunc(), onFifth.ToFunc(), onSixth.ToFunc());

        public static void MatchVoid<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> o, Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird, Action<T4> onFourth, Action<T5> onFifth, Action<T6> onSixth, Action<T7> onSeventh) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            o.Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc(), onFourth.ToFunc(), onFifth.ToFunc(), onSixth.ToFunc(), onSeventh.ToFunc());

        public static void MatchVoid<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> o, Action<T1> onFirst, Action<T2> onSecond, Action<T3> onThird, Action<T4> onFourth, Action<T5> onFifth, Action<T6> onSixth, Action<T7> onSeventh, Action<T8> onEighth) =>
            // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
            o.Match(onFirst.ToFunc(), onSecond.ToFunc(), onThird.ToFunc(), onFourth.ToFunc(), onFifth.ToFunc(), onSixth.ToFunc(), onSeventh.ToFunc(), onEighth.ToFunc());

        public static Task<T> MatchAsync<T, T1, T2>(this Either<T1, T2> o, Func<T1, Task<T>> onFirst, Func<T2, Task<T>> onSecond) =>
            o.Match(onFirst, onSecond);

        public static Task<T> MatchAsync<T, T1, T2, T3>(this Either<T1, T2, T3> o, Func<T1, Task<T>> onFirst, Func<T2, Task<T>> onSecond, Func<T3, Task<T>> onThird) =>
            o.Match(onFirst, onSecond, onThird);

        public static Task<T> MatchAsync<T, T1, T2, T3, T4>(this Either<T1, T2, T3, T4> o, Func<T1, Task<T>> onFirst, Func<T2, Task<T>> onSecond, Func<T3, Task<T>> onThird, Func<T4, Task<T>> onFourth) =>
            o.Match(onFirst, onSecond, onThird, onFourth);

        public static Task<T> MatchAsync<T, T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> o, Func<T1, Task<T>> onFirst, Func<T2, Task<T>> onSecond, Func<T3, Task<T>> onThird, Func<T4, Task<T>> onFourth, Func<T5, Task<T>> onFifth) =>
            o.Match(onFirst, onSecond, onThird, onFourth, onFifth);

        public static Task<T> MatchAsync<T, T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> o, Func<T1, Task<T>> onFirst, Func<T2, Task<T>> onSecond, Func<T3, Task<T>> onThird, Func<T4, Task<T>> onFourth, Func<T5, Task<T>> onFifth, Func<T6, Task<T>> onSixth) =>
            o.Match(onFirst, onSecond, onThird, onFourth, onFifth, onSixth);

        public static Task<T> MatchAsync<T, T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> o, Func<T1, Task<T>> onFirst, Func<T2, Task<T>> onSecond, Func<T3, Task<T>> onThird, Func<T4, Task<T>> onFourth, Func<T5, Task<T>> onFifth, Func<T6, Task<T>> onSixth, Func<T7, Task<T>> onSeventh) =>
            o.Match(onFirst, onSecond, onThird, onFourth, onFifth, onSixth, onSeventh);

        public static Task<T> MatchAsync<T, T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> o, Func<T1, Task<T>> onFirst, Func<T2, Task<T>> onSecond, Func<T3, Task<T>> onThird, Func<T4, Task<T>> onFourth, Func<T5, Task<T>> onFifth, Func<T6, Task<T>> onSixth, Func<T7, Task<T>> onSeventh, Func<T8, Task<T>> onEighth) =>
            o.Match(onFirst, onSecond, onThird, onFourth, onFifth, onSixth, onSeventh, onEighth);

        public static Task MatchAsyncVoid<T1, T2>(this Either<T1, T2> o, Func<T1, Task> onFirst, Func<T2, Task> onSecond) =>
            o.MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc());

        public static Task MatchAsyncVoid<T1, T2, T3>(this Either<T1, T2, T3> o, Func<T1, Task> onFirst, Func<T2, Task> onSecond, Func<T3, Task> onThird) =>
            o.MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc(), onThird.ToAsyncFunc());

        public static Task MatchAsyncVoid<T1, T2, T3, T4>(this Either<T1, T2, T3, T4> o, Func<T1, Task> onFirst, Func<T2, Task> onSecond, Func<T3, Task> onThird, Func<T4, Task> onFourth) =>
            o.MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc(), onThird.ToAsyncFunc(), onFourth.ToAsyncFunc());

        public static Task MatchAsyncVoid<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> o, Func<T1, Task> onFirst, Func<T2, Task> onSecond, Func<T3, Task> onThird, Func<T4, Task> onFourth, Func<T5, Task> onFifth) =>
            o.MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc(), onThird.ToAsyncFunc(), onFourth.ToAsyncFunc(), onFifth.ToAsyncFunc());

        public static Task MatchAsyncVoid<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> o, Func<T1, Task> onFirst, Func<T2, Task> onSecond, Func<T3, Task> onThird, Func<T4, Task> onFourth, Func<T5, Task> onFifth, Func<T6, Task> onSixth) =>
            o.MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc(), onThird.ToAsyncFunc(), onFourth.ToAsyncFunc(), onFifth.ToAsyncFunc(), onSixth.ToAsyncFunc());

        public static Task MatchAsyncVoid<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> o, Func<T1, Task> onFirst, Func<T2, Task> onSecond, Func<T3, Task> onThird, Func<T4, Task> onFourth, Func<T5, Task> onFifth, Func<T6, Task> onSixth, Func<T7, Task> onSeventh) =>
            o.MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc(), onThird.ToAsyncFunc(), onFourth.ToAsyncFunc(), onFifth.ToAsyncFunc(), onSixth.ToAsyncFunc(), onSeventh.ToAsyncFunc());

        public static Task MatchAsyncVoid<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> o, Func<T1, Task> onFirst, Func<T2, Task> onSecond, Func<T3, Task> onThird, Func<T4, Task> onFourth, Func<T5, Task> onFifth, Func<T6, Task> onSixth, Func<T7, Task> onSeventh, Func<T8, Task> onEighth) =>
            o.MatchAsync(onFirst.ToAsyncFunc(), onSecond.ToAsyncFunc(), onThird.ToAsyncFunc(), onFourth.ToAsyncFunc(), onFifth.ToAsyncFunc(), onSixth.ToAsyncFunc(), onSeventh.ToAsyncFunc(), onEighth.ToAsyncFunc());

        public static Either<T, T2> MapFirst<T1, T2, T>(this Either<T1, T2> a, Func<T1, T> f) =>
            a.Match(v1 => Either<T, T2>.First(f(v1)), v2 => Either.Second(v2));

        public static Either<T1, T> MapSecond<T1, T2, T>(this Either<T1, T2> a, Func<T2, T> f) =>
            a.Match(Either<T1, T>.First, v2 => Either.Second(f(v2)));

        public static Either<T, T2, T3> MapFirst<T1, T2, T3, T>(this Either<T1, T2, T3> a, Func<T1, T> f) =>
            a.Match(v1 => Either<T, T2, T3>.First(f(v1)), v2 => Either.Second(v2), v3 => Either.Third(v3));

        public static Either<T1, T, T3> MapSecond<T1, T2, T3, T>(this Either<T1, T2, T3> a, Func<T2, T> f) =>
            a.Match(Either<T1, T, T3>.First, v2 => Either.Second(f(v2)), v3 => Either.Third(v3));

        public static Either<T1, T2, T> MapThird<T1, T2, T3, T>(this Either<T1, T2, T3> a, Func<T3, T> f) =>
            a.Match(Either<T1, T2, T>.First, v2 => Either.Second(v2), v3 => Either.Third(f(v3)));

        public static Either<T, T2, T3, T4> MapFirst<T1, T2, T3, T4, T>(this Either<T1, T2, T3, T4> a, Func<T1, T> f) =>
            a.Match(v1 => Either<T, T2, T3, T4>.First(f(v1)), v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4));

        public static Either<T1, T, T3, T4> MapSecond<T1, T2, T3, T4, T>(this Either<T1, T2, T3, T4> a, Func<T2, T> f) =>
            a.Match(Either<T1, T, T3, T4>.First, v2 => Either.Second(f(v2)), v3 => Either.Third(v3), v4 => Either.Fourth(v4));

        public static Either<T1, T2, T, T4> MapThird<T1, T2, T3, T4, T>(this Either<T1, T2, T3, T4> a, Func<T3, T> f) =>
            a.Match(Either<T1, T2, T, T4>.First, v2 => Either.Second(v2), v3 => Either.Third(f(v3)), v4 => Either.Fourth(v4));

        public static Either<T1, T2, T3, T> MapFourth<T1, T2, T3, T4, T>(this Either<T1, T2, T3, T4> a, Func<T4, T> f) =>
            a.Match(Either<T1, T2, T3, T>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(f(v4)));

        public static Either<T, T2, T3, T4, T5> MapFirst<T1, T2, T3, T4, T5, T>(this Either<T1, T2, T3, T4, T5> a, Func<T1, T> f) =>
            a.Match(v1 => Either<T, T2, T3, T4, T5>.First(f(v1)), v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5));

        public static Either<T1, T, T3, T4, T5> MapSecond<T1, T2, T3, T4, T5, T>(this Either<T1, T2, T3, T4, T5> a, Func<T2, T> f) =>
            a.Match(Either<T1, T, T3, T4, T5>.First, v2 => Either.Second(f(v2)), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5));

        public static Either<T1, T2, T, T4, T5> MapThird<T1, T2, T3, T4, T5, T>(this Either<T1, T2, T3, T4, T5> a, Func<T3, T> f) =>
            a.Match(Either<T1, T2, T, T4, T5>.First, v2 => Either.Second(v2), v3 => Either.Third(f(v3)), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5));

        public static Either<T1, T2, T3, T, T5> MapFourth<T1, T2, T3, T4, T5, T>(this Either<T1, T2, T3, T4, T5> a, Func<T4, T> f) =>
            a.Match(Either<T1, T2, T3, T, T5>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(f(v4)), v5 => Either.Fifth(v5));

        public static Either<T1, T2, T3, T4, T> MapFifth<T1, T2, T3, T4, T5, T>(this Either<T1, T2, T3, T4, T5> a, Func<T5, T> f) =>
            a.Match(Either<T1, T2, T3, T4, T>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(f(v5)));

        public static Either<T, T2, T3, T4, T5, T6> MapFirst<T1, T2, T3, T4, T5, T6, T>(this Either<T1, T2, T3, T4, T5, T6> a, Func<T1, T> f) =>
            a.Match(v1 => Either<T, T2, T3, T4, T5, T6>.First(f(v1)), v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6));

        public static Either<T1, T, T3, T4, T5, T6> MapSecond<T1, T2, T3, T4, T5, T6, T>(this Either<T1, T2, T3, T4, T5, T6> a, Func<T2, T> f) =>
            a.Match(Either<T1, T, T3, T4, T5, T6>.First, v2 => Either.Second(f(v2)), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6));

        public static Either<T1, T2, T, T4, T5, T6> MapThird<T1, T2, T3, T4, T5, T6, T>(this Either<T1, T2, T3, T4, T5, T6> a, Func<T3, T> f) =>
            a.Match(Either<T1, T2, T, T4, T5, T6>.First, v2 => Either.Second(v2), v3 => Either.Third(f(v3)), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6));

        public static Either<T1, T2, T3, T, T5, T6> MapFourth<T1, T2, T3, T4, T5, T6, T>(this Either<T1, T2, T3, T4, T5, T6> a, Func<T4, T> f) =>
            a.Match(Either<T1, T2, T3, T, T5, T6>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(f(v4)), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6));

        public static Either<T1, T2, T3, T4, T, T6> MapFifth<T1, T2, T3, T4, T5, T6, T>(this Either<T1, T2, T3, T4, T5, T6> a, Func<T5, T> f) =>
            a.Match(Either<T1, T2, T3, T4, T, T6>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(f(v5)), v6 => Either.Sixth(v6));

        public static Either<T1, T2, T3, T4, T5, T> MapSixth<T1, T2, T3, T4, T5, T6, T>(this Either<T1, T2, T3, T4, T5, T6> a, Func<T6, T> f) =>
            a.Match(Either<T1, T2, T3, T4, T5, T>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(f(v6)));

        public static Either<T, T2, T3, T4, T5, T6, T7> MapFirst<T1, T2, T3, T4, T5, T6, T7, T>(this Either<T1, T2, T3, T4, T5, T6, T7> a, Func<T1, T> f) =>
            a.Match(v1 => Either<T, T2, T3, T4, T5, T6, T7>.First(f(v1)), v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7));

        public static Either<T1, T, T3, T4, T5, T6, T7> MapSecond<T1, T2, T3, T4, T5, T6, T7, T>(this Either<T1, T2, T3, T4, T5, T6, T7> a, Func<T2, T> f) =>
            a.Match(Either<T1, T, T3, T4, T5, T6, T7>.First, v2 => Either.Second(f(v2)), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7));

        public static Either<T1, T2, T, T4, T5, T6, T7> MapThird<T1, T2, T3, T4, T5, T6, T7, T>(this Either<T1, T2, T3, T4, T5, T6, T7> a, Func<T3, T> f) =>
            a.Match(Either<T1, T2, T, T4, T5, T6, T7>.First, v2 => Either.Second(v2), v3 => Either.Third(f(v3)), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7));

        public static Either<T1, T2, T3, T, T5, T6, T7> MapFourth<T1, T2, T3, T4, T5, T6, T7, T>(this Either<T1, T2, T3, T4, T5, T6, T7> a, Func<T4, T> f) =>
            a.Match(Either<T1, T2, T3, T, T5, T6, T7>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(f(v4)), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7));

        public static Either<T1, T2, T3, T4, T, T6, T7> MapFifth<T1, T2, T3, T4, T5, T6, T7, T>(this Either<T1, T2, T3, T4, T5, T6, T7> a, Func<T5, T> f) =>
            a.Match(Either<T1, T2, T3, T4, T, T6, T7>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(f(v5)), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7));

        public static Either<T1, T2, T3, T4, T5, T, T7> MapSixth<T1, T2, T3, T4, T5, T6, T7, T>(this Either<T1, T2, T3, T4, T5, T6, T7> a, Func<T6, T> f) =>
            a.Match(Either<T1, T2, T3, T4, T5, T, T7>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(f(v6)), v7 => Either.Seventh(v7));

        public static Either<T1, T2, T3, T4, T5, T6, T> MapSeventh<T1, T2, T3, T4, T5, T6, T7, T>(this Either<T1, T2, T3, T4, T5, T6, T7> a, Func<T7, T> f) =>
            a.Match(Either<T1, T2, T3, T4, T5, T6, T>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(f(v7)));

        public static Either<T, T2, T3, T4, T5, T6, T7, T8> MapFirst<T1, T2, T3, T4, T5, T6, T7, T8, T>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a, Func<T1, T> f) =>
            a.Match(v1 => Either<T, T2, T3, T4, T5, T6, T7, T8>.First(f(v1)), v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7), v8 => Either.Eighth(v8));

        public static Either<T1, T, T3, T4, T5, T6, T7, T8> MapSecond<T1, T2, T3, T4, T5, T6, T7, T8, T>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a, Func<T2, T> f) =>
            a.Match(Either<T1, T, T3, T4, T5, T6, T7, T8>.First, v2 => Either.Second(f(v2)), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7), v8 => Either.Eighth(v8));

        public static Either<T1, T2, T, T4, T5, T6, T7, T8> MapThird<T1, T2, T3, T4, T5, T6, T7, T8, T>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a, Func<T3, T> f) =>
            a.Match(Either<T1, T2, T, T4, T5, T6, T7, T8>.First, v2 => Either.Second(v2), v3 => Either.Third(f(v3)), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7), v8 => Either.Eighth(v8));

        public static Either<T1, T2, T3, T, T5, T6, T7, T8> MapFourth<T1, T2, T3, T4, T5, T6, T7, T8, T>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a, Func<T4, T> f) =>
            a.Match(Either<T1, T2, T3, T, T5, T6, T7, T8>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(f(v4)), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7), v8 => Either.Eighth(v8));

        public static Either<T1, T2, T3, T4, T, T6, T7, T8> MapFifth<T1, T2, T3, T4, T5, T6, T7, T8, T>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a, Func<T5, T> f) =>
            a.Match(Either<T1, T2, T3, T4, T, T6, T7, T8>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(f(v5)), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7), v8 => Either.Eighth(v8));

        public static Either<T1, T2, T3, T4, T5, T, T7, T8> MapSixth<T1, T2, T3, T4, T5, T6, T7, T8, T>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a, Func<T6, T> f) =>
            a.Match(Either<T1, T2, T3, T4, T5, T, T7, T8>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(f(v6)), v7 => Either.Seventh(v7), v8 => Either.Eighth(v8));

        public static Either<T1, T2, T3, T4, T5, T6, T, T8> MapSeventh<T1, T2, T3, T4, T5, T6, T7, T8, T>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a, Func<T7, T> f) =>
            a.Match(Either<T1, T2, T3, T4, T5, T6, T, T8>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(f(v7)), v8 => Either.Eighth(v8));

        public static Either<T1, T2, T3, T4, T5, T6, T7, T> MapEighth<T1, T2, T3, T4, T5, T6, T7, T8, T>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a, Func<T8, T> f) =>
            a.Match(Either<T1, T2, T3, T4, T5, T6, T7, T>.First, v2 => Either.Second(v2), v3 => Either.Third(v3), v4 => Either.Fourth(v4), v5 => Either.Fifth(v5), v6 => Either.Sixth(v6), v7 => Either.Seventh(v7), v8 => Either.Eighth(f(v8)));

        public static Maybe<T1> TryGetFirst<T1, T2>(this Either<T1, T2> a) =>
            a.Match(Maybe.Some, _ => Maybe.None);

        public static Maybe<T2> TryGetSecond<T1, T2>(this Either<T1, T2> a) =>
            a.Match(_ => Maybe.None, Maybe.Some);

        public static Maybe<T1> TryGetFirst<T1, T2, T3>(this Either<T1, T2, T3> a) =>
            a.Match(Maybe.Some, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T2> TryGetSecond<T1, T2, T3>(this Either<T1, T2, T3> a) =>
            a.Match(_ => Maybe.None, Maybe.Some, _ => Maybe.None);

        public static Maybe<T3> TryGetThird<T1, T2, T3>(this Either<T1, T2, T3> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, Maybe.Some);

        public static Maybe<T1> TryGetFirst<T1, T2, T3, T4>(this Either<T1, T2, T3, T4> a) =>
            a.Match(Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T2> TryGetSecond<T1, T2, T3, T4>(this Either<T1, T2, T3, T4> a) =>
            a.Match(_ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T3> TryGetThird<T1, T2, T3, T4>(this Either<T1, T2, T3, T4> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None);

        public static Maybe<T4> TryGetFourth<T1, T2, T3, T4>(this Either<T1, T2, T3, T4> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some);

        public static Maybe<T1> TryGetFirst<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a) =>
            a.Match(Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T2> TryGetSecond<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a) =>
            a.Match(_ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T3> TryGetThird<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T4> TryGetFourth<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None);

        public static Maybe<T5> TryGetFifth<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some);

        public static Maybe<T1> TryGetFirst<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T2> TryGetSecond<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T3> TryGetThird<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T4> TryGetFourth<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T5> TryGetFifth<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None);

        public static Maybe<T6> TryGetSixth<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some);

        public static Maybe<T1> TryGetFirst<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T2> TryGetSecond<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T3> TryGetThird<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T4> TryGetFourth<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T5> TryGetFifth<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T6> TryGetSixth<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None);

        public static Maybe<T7> TryGetSeventh<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some);

        public static Maybe<T1> TryGetFirst<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T2> TryGetSecond<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T3> TryGetThird<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T4> TryGetFourth<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T5> TryGetFifth<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T6> TryGetSixth<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None, _ => Maybe.None);

        public static Maybe<T7> TryGetSeventh<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some, _ => Maybe.None);

        public static Maybe<T8> TryGetEighth<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, _ => Maybe.None, Maybe.Some);

        public static bool IsFirst<T1, T2>(this Either<T1, T2> a) =>
            a.Match(_ => true, _ => false);

        public static bool IsSecond<T1, T2>(this Either<T1, T2> a) =>
            a.Match(_ => false, _ => true);

        public static bool IsFirst<T1, T2, T3>(this Either<T1, T2, T3> a) =>
            a.Match(_ => true, _ => false, _ => false);

        public static bool IsSecond<T1, T2, T3>(this Either<T1, T2, T3> a) =>
            a.Match(_ => false, _ => true, _ => false);

        public static bool IsThird<T1, T2, T3>(this Either<T1, T2, T3> a) =>
            a.Match(_ => false, _ => false, _ => true);

        public static bool IsFirst<T1, T2, T3, T4>(this Either<T1, T2, T3, T4> a) =>
            a.Match(_ => true, _ => false, _ => false, _ => false);

        public static bool IsSecond<T1, T2, T3, T4>(this Either<T1, T2, T3, T4> a) =>
            a.Match(_ => false, _ => true, _ => false, _ => false);

        public static bool IsThird<T1, T2, T3, T4>(this Either<T1, T2, T3, T4> a) =>
            a.Match(_ => false, _ => false, _ => true, _ => false);

        public static bool IsFourth<T1, T2, T3, T4>(this Either<T1, T2, T3, T4> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => true);

        public static bool IsFirst<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a) =>
            a.Match(_ => true, _ => false, _ => false, _ => false, _ => false);

        public static bool IsSecond<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a) =>
            a.Match(_ => false, _ => true, _ => false, _ => false, _ => false);

        public static bool IsThird<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a) =>
            a.Match(_ => false, _ => false, _ => true, _ => false, _ => false);

        public static bool IsFourth<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => true, _ => false);

        public static bool IsFifth<T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => false, _ => true);

        public static bool IsFirst<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => true, _ => false, _ => false, _ => false, _ => false, _ => false);

        public static bool IsSecond<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => false, _ => true, _ => false, _ => false, _ => false, _ => false);

        public static bool IsThird<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => false, _ => false, _ => true, _ => false, _ => false, _ => false);

        public static bool IsFourth<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => true, _ => false, _ => false);

        public static bool IsFifth<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => false, _ => true, _ => false);

        public static bool IsSixth<T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => true);

        public static bool IsFirst<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => true, _ => false, _ => false, _ => false, _ => false, _ => false, _ => false);

        public static bool IsSecond<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => false, _ => true, _ => false, _ => false, _ => false, _ => false, _ => false);

        public static bool IsThird<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => false, _ => false, _ => true, _ => false, _ => false, _ => false, _ => false);

        public static bool IsFourth<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => true, _ => false, _ => false, _ => false);

        public static bool IsFifth<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => false, _ => true, _ => false, _ => false);

        public static bool IsSixth<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => true, _ => false);

        public static bool IsSeventh<T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => false, _ => true);

        public static bool IsFirst<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => true, _ => false, _ => false, _ => false, _ => false, _ => false, _ => false, _ => false);

        public static bool IsSecond<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => false, _ => true, _ => false, _ => false, _ => false, _ => false, _ => false, _ => false);

        public static bool IsThird<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => false, _ => false, _ => true, _ => false, _ => false, _ => false, _ => false, _ => false);

        public static bool IsFourth<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => true, _ => false, _ => false, _ => false, _ => false);

        public static bool IsFifth<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => false, _ => true, _ => false, _ => false, _ => false);

        public static bool IsSixth<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => true, _ => false, _ => false);

        public static bool IsSeventh<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => false, _ => true, _ => false);

        public static bool IsEighth<T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a) =>
            a.Match(_ => false, _ => false, _ => false, _ => false, _ => false, _ => false, _ => false, _ => true);

        public static T GetValueAs<T, T1, T2>(this Either<T1, T2> a)
            where T1 : T
            where T2 : T =>
            a.Match<T>(v1 => v1, v2 => v2);

        public static T GetValueAs<T, T1, T2, T3>(this Either<T1, T2, T3> a)
            where T1 : T
            where T2 : T
            where T3 : T =>
            a.Match<T>(v1 => v1, v2 => v2, v3 => v3);

        public static T GetValueAs<T, T1, T2, T3, T4>(this Either<T1, T2, T3, T4> a)
            where T1 : T
            where T2 : T
            where T3 : T
            where T4 : T =>
            a.Match<T>(v1 => v1, v2 => v2, v3 => v3, v4 => v4);

        public static T GetValueAs<T, T1, T2, T3, T4, T5>(this Either<T1, T2, T3, T4, T5> a)
            where T1 : T
            where T2 : T
            where T3 : T
            where T4 : T
            where T5 : T =>
            a.Match<T>(v1 => v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5);

        public static T GetValueAs<T, T1, T2, T3, T4, T5, T6>(this Either<T1, T2, T3, T4, T5, T6> a)
            where T1 : T
            where T2 : T
            where T3 : T
            where T4 : T
            where T5 : T
            where T6 : T =>
            a.Match<T>(v1 => v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5, v6 => v6);

        public static T GetValueAs<T, T1, T2, T3, T4, T5, T6, T7>(this Either<T1, T2, T3, T4, T5, T6, T7> a)
            where T1 : T
            where T2 : T
            where T3 : T
            where T4 : T
            where T5 : T
            where T6 : T
            where T7 : T =>
            a.Match<T>(v1 => v1, v2 => v2, v3 => v3, v4 => v4, v5 => v5, v6 => v6, v7 => v7);

        public static T GetValueAs<T, T1, T2, T3, T4, T5, T6, T7, T8>(this Either<T1, T2, T3, T4, T5, T6, T7, T8> a)
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