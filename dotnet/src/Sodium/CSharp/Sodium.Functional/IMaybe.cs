using System;
using System.Threading.Tasks;

namespace Sodium.Functional
{
    public interface IMaybe
    {
        T Match<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, T> onSome,
            [JetBrains.Annotations.InstantHandle] Func<T> onNone);
        
        void MatchVoid(
            [JetBrains.Annotations.InstantHandle] Action<object> onSome,
            [JetBrains.Annotations.InstantHandle] Action onNone);
        
        void MatchSome([JetBrains.Annotations.InstantHandle] Action<object> onSome);
        void MatchNone([JetBrains.Annotations.InstantHandle] Action onNone);
        
        Task<T> MatchAsync<T>(
            [JetBrains.Annotations.InstantHandle] Func<object, Task<T>> onSome,
            [JetBrains.Annotations.InstantHandle] Func<Task<T>> onNone);
        
        Task MatchAsyncVoid(
            [JetBrains.Annotations.InstantHandle] Func<object, Task> onSome,
            [JetBrains.Annotations.InstantHandle] Func<Task> onNone);
        
        Task MatchSomeAsync([JetBrains.Annotations.InstantHandle] Func<object, Task> onSome);
        Task MatchNoneAsync([JetBrains.Annotations.InstantHandle] Func<Task> onNone);
    }
}