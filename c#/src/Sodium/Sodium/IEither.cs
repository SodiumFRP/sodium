using System;
using System.Threading.Tasks;

namespace Sodium
{
    public interface IEither
    {
        object GetValueAsObject();
    }

    public interface IEitherOfTwo : IEither
    {
        T Match<T>(Func<object, T> onFirst, Func<object, T> onSecond);
        void MatchVoid(Action<object> onFirst, Action<object> onSecond);
        Task<T> MatchAsync<T>(Func<object, Task<T>> onFirst, Func<object, Task<T>> onSecond);
        Task MatchAsyncVoid(Func<object, Task> onFirst, Func<object, Task> onSecond);
    }

    public interface IEitherOfThree : IEither
    {
        T Match<T>(Func<object, T> onFirst, Func<object, T> onSecond, Func<object, T> onThird);
        void MatchVoid(Action<object> onFirst, Action<object> onSecond, Action<object> onThird);

        Task<T> MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird);

        Task MatchAsyncVoid(Func<object, Task> onFirst, Func<object, Task> onSecond, Func<object, Task> onThird);
    }

    public interface IEitherOfFour : IEither
    {
        T Match<T>(
            Func<object, T> onFirst,
            Func<object, T> onSecond,
            Func<object, T> onThird,
            Func<object, T> onFourth);

        void MatchVoid(
            Action<object> onFirst,
            Action<object> onSecond,
            Action<object> onThird,
            Action<object> onFourth);

        Task<T> MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird,
            Func<object, Task<T>> onFourth);

        Task MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird,
            Func<object, Task> onFourth);
    }

    public interface IEitherOfFive : IEither
    {
        T Match<T>(
            Func<object, T> onFirst,
            Func<object, T> onSecond,
            Func<object, T> onThird,
            Func<object, T> onFourth,
            Func<object, T> onFifth);

        void MatchVoid(
            Action<object> onFirst,
            Action<object> onSecond,
            Action<object> onThird,
            Action<object> onFourth,
            Action<object> onFifth);

        Task<T> MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird,
            Func<object, Task<T>> onFourth,
            Func<object, Task<T>> onFifth);

        Task MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird,
            Func<object, Task> onFourth,
            Func<object, Task> onFifth);
    }

    public interface IEitherOfSix : IEither
    {
        T Match<T>(
            Func<object, T> onFirst,
            Func<object, T> onSecond,
            Func<object, T> onThird,
            Func<object, T> onFourth,
            Func<object, T> onFifth,
            Func<object, T> onSixth);

        void MatchVoid(
            Action<object> onFirst,
            Action<object> onSecond,
            Action<object> onThird,
            Action<object> onFourth,
            Action<object> onFifth,
            Action<object> onSixth);

        Task<T> MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird,
            Func<object, Task<T>> onFourth,
            Func<object, Task<T>> onFifth,
            Func<object, Task<T>> onSixth);

        Task MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird,
            Func<object, Task> onFourth,
            Func<object, Task> onFifth,
            Func<object, Task> onSixth);
    }

    public interface IEitherOfSeven : IEither
    {
        T Match<T>(
            Func<object, T> onFirst,
            Func<object, T> onSecond,
            Func<object, T> onThird,
            Func<object, T> onFourth,
            Func<object, T> onFifth,
            Func<object, T> onSixth,
            Func<object, T> onSeventh);

        void MatchVoid(
            Action<object> onFirst,
            Action<object> onSecond,
            Action<object> onThird,
            Action<object> onFourth,
            Action<object> onFifth,
            Action<object> onSixth,
            Action<object> onSeventh);

        Task<T> MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird,
            Func<object, Task<T>> onFourth,
            Func<object, Task<T>> onFifth,
            Func<object, Task<T>> onSixth,
            Func<object, Task<T>> onSeventh);

        Task MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird,
            Func<object, Task> onFourth,
            Func<object, Task> onFifth,
            Func<object, Task> onSixth,
            Func<object, Task> onSeventh);
    }

    public interface IEitherOfEight : IEither
    {
        T Match<T>(
            Func<object, T> onFirst,
            Func<object, T> onSecond,
            Func<object, T> onThird,
            Func<object, T> onFourth,
            Func<object, T> onFifth,
            Func<object, T> onSixth,
            Func<object, T> onSeventh,
            Func<object, T> onEighth);

        void MatchVoid(
            Action<object> onFirst,
            Action<object> onSecond,
            Action<object> onThird,
            Action<object> onFourth,
            Action<object> onFifth,
            Action<object> onSixth,
            Action<object> onSeventh,
            Action<object> onEighth);

        Task<T> MatchAsync<T>(
            Func<object, Task<T>> onFirst,
            Func<object, Task<T>> onSecond,
            Func<object, Task<T>> onThird,
            Func<object, Task<T>> onFourth,
            Func<object, Task<T>> onFifth,
            Func<object, Task<T>> onSixth,
            Func<object, Task<T>> onSeventh,
            Func<object, Task<T>> onEighth);

        Task MatchAsyncVoid(
            Func<object, Task> onFirst,
            Func<object, Task> onSecond,
            Func<object, Task> onThird,
            Func<object, Task> onFourth,
            Func<object, Task> onFifth,
            Func<object, Task> onSixth,
            Func<object, Task> onSeventh,
            Func<object, Task> onEighth);
    }
}