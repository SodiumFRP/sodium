using System;
using System.Threading.Tasks;

namespace Sodium.Functional
{
    public interface IEither
    {
        [JetBrains.Annotations.Pure]
        object GetValueAsObject();
    }

    public interface IEitherOfTwo : IEither
    {
        T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSecond);
        
        void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<object> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<object> onSecond);
        
        Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSecond);
        
        Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSecond);
    }

    public interface IEitherOfThree : IEither
    {
        T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onThird);
        
        void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<object> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<object> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<object> onThird);

        Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onThird);

        Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onThird);
    }

    public interface IEitherOfFour : IEither
    {
        T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFourth);

        void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<object> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<object> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<object> onThird,
            [JetBrains.Annotations.InstantHandle] Action<object> onFourth);

        Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFourth);

        Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFourth);
    }

    public interface IEitherOfFive : IEither
    {
        T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFifth);

        void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<object> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<object> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<object> onThird,
            [JetBrains.Annotations.InstantHandle] Action<object> onFourth,
            [JetBrains.Annotations.InstantHandle] Action<object> onFifth);

        Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFifth);

        Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFifth);
    }

    public interface IEitherOfSix : IEither
    {
        T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSixth);

        void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<object> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<object> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<object> onThird,
            [JetBrains.Annotations.InstantHandle] Action<object> onFourth,
            [JetBrains.Annotations.InstantHandle] Action<object> onFifth,
            [JetBrains.Annotations.InstantHandle] Action<object> onSixth);

        Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSixth);

        Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSixth);
    }

    public interface IEitherOfSeven : IEither
    {
        T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSeventh);

        void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<object> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<object> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<object> onThird,
            [JetBrains.Annotations.InstantHandle] Action<object> onFourth,
            [JetBrains.Annotations.InstantHandle] Action<object> onFifth,
            [JetBrains.Annotations.InstantHandle] Action<object> onSixth,
            [JetBrains.Annotations.InstantHandle] Action<object> onSeventh);

        Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSeventh);

        Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSeventh);
    }

    public interface IEitherOfEight : IEither
    {
        T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSeventh,
            [JetBrains.Annotations.InstantHandle] Func<object, T> onEighth);

        void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<object> onFirst,
            [JetBrains.Annotations.InstantHandle] Action<object> onSecond,
            [JetBrains.Annotations.InstantHandle] Action<object> onThird,
            [JetBrains.Annotations.InstantHandle] Action<object> onFourth,
            [JetBrains.Annotations.InstantHandle] Action<object> onFifth,
            [JetBrains.Annotations.InstantHandle] Action<object> onSixth,
            [JetBrains.Annotations.InstantHandle] Action<object> onSeventh,
            [JetBrains.Annotations.InstantHandle] Action<object> onEighth);

        Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSeventh,
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onEighth);

        Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFirst,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSecond,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onThird,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFourth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onFifth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSixth,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSeventh,
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onEighth);
    }
}