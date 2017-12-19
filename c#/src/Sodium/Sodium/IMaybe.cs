using System;
using System.Threading.Tasks;

namespace Sodium
{
    public interface IMaybe
    {
        T Match<T>(Func<object, T> onSome, Func<T> onNone);
        void MatchVoid(Action<object> onSome, Action onNone);
        void MatchSome(Action<object> onSome);
        void MatchNone(Action onNone);
        Task<T> MatchAsync<T>(Func<object, Task<T>> onSome, Func<Task<T>> onNone);
        Task MatchAsyncVoid(Func<object, Task> onSome, Func<Task> onNone);
        Task MatchSomeAsync(Func<object, Task> onSome);
        Task MatchNoneAsync(Func<Task> onNone);
    }
}