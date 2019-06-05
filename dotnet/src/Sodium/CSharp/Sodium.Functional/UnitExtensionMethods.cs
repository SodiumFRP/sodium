using System;
using System.Threading.Tasks;

namespace Sodium.Functional
{
    public static class UnitExtensionMethods
    {
        public static Unit Ignore<T>(this T o) => Unit.Value;
        public static T Upcast<T>(this T o) => o;

        public static Func<Unit> ToFunc(this Action action) =>
            () =>
            {
                action();
                return Unit.Value;
            };

        public static Func<T, Unit> ToFunc<T>(this Action<T> action) =>
            v =>
            {
                action(v);
                return Unit.Value;
            };

        public static Func<T1, T2, Unit> ToFunc<T1, T2>(this Action<T1, T2> action) =>
            (v1, v2) =>
            {
                action(v1, v2);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, Unit> ToFunc<T1, T2, T3>(this Action<T1, T2, T3> action) =>
            (v1, v2, v3) =>
            {
                action(v1, v2, v3);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, Unit> ToFunc<T1, T2, T3, T4>(this Action<T1, T2, T3, T4> action) =>
            (v1, v2, v3, v4) =>
            {
                action(v1, v2, v3, v4);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, Unit> ToFunc<T1, T2, T3, T4, T5>(
            this Action<T1, T2, T3, T4, T5> action) =>
            (v1, v2, v3, v4, v5) =>
            {
                action(v1, v2, v3, v4, v5);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, Unit> ToFunc<T1, T2, T3, T4, T5, T6>(
            this Action<T1, T2, T3, T4, T5, T6> action) =>
            (v1, v2, v3, v4, v5, v6) =>
            {
                action(v1, v2, v3, v4, v5, v6);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, Unit> ToFunc<T1, T2, T3, T4, T5, T6, T7>(
            this Action<T1, T2, T3, T4, T5, T6, T7> action) =>
            (v1, v2, v3, v4, v5, v6, v7) =>
            {
                action(v1, v2, v3, v4, v5, v6, v7);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, Unit> ToFunc<T1, T2, T3, T4, T5, T6, T7, T8>(
            this Action<T1, T2, T3, T4, T5, T6, T7, T8> action) =>
            (v1, v2, v3, v4, v5, v6, v7, v8) =>
            {
                action(v1, v2, v3, v4, v5, v6, v7, v8);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, Unit> ToFunc<T1, T2, T3, T4, T5, T6, T7, T8, T9>(
            this Action<T1, T2, T3, T4, T5, T6, T7, T8, T9> action) =>
            (v1, v2, v3, v4, v5, v6, v7, v8, v9) =>
            {
                action(v1, v2, v3, v4, v5, v6, v7, v8, v9);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, Unit> ToFunc<T1, T2, T3, T4, T5, T6, T7, T8, T9,
            T10>(this Action<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> action) =>
            (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) =>
            {
                action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, Unit> ToFunc<T1, T2, T3, T4, T5, T6, T7, T8,
            T9, T10, T11>(this Action<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11> action) =>
            (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) =>
            {
                action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, Unit> ToFunc<T1, T2, T3, T4, T5, T6, T7,
            T8, T9, T10, T11, T12>(this Action<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12> action) =>
            (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) =>
            {
                action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, Unit> ToFunc<T1, T2, T3, T4, T5, T6,
            T7, T8, T9, T10, T11, T12, T13>(
            this Action<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13> action) =>
            (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) =>
            {
                action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, Unit> ToFunc<T1, T2, T3, T4, T5,
            T6, T7, T8, T9, T10, T11, T12, T13, T14>(
            this Action<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14> action) =>
            (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) =>
            {
                action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Unit> ToFunc<T1, T2, T3,
            T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15>(
            this Action<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15> action) =>
            (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) =>
            {
                action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, Unit> ToFunc<T1, T2,
            T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16>(
            this Action<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16> action) =>
            (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) =>
            {
                action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16);
                return Unit.Value;
            };

        public static Func<Task<Unit>> ToAsyncFunc(this Func<Task> action) =>
            async () =>
            {
                await action();
                return Unit.Value;
            };

        public static Func<T, Task<Unit>> ToAsyncFunc<T>(this Func<T, Task> action) =>
            async v =>
            {
                await action(v);
                return Unit.Value;
            };

        public static Func<T1, T2, Task<Unit>> ToAsyncFunc<T1, T2>(this Func<T1, T2, Task> action) =>
            async (v1, v2) =>
            {
                await action(v1, v2);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, Task<Unit>> ToAsyncFunc<T1, T2, T3>(this Func<T1, T2, T3, Task> action) =>
            async (v1, v2, v3) =>
            {
                await action(v1, v2, v3);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, Task<Unit>> ToAsyncFunc<T1, T2, T3, T4>(
            this Func<T1, T2, T3, T4, Task> action) =>
            async (v1, v2, v3, v4) =>
            {
                await action(v1, v2, v3, v4);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, Task<Unit>> ToAsyncFunc<T1, T2, T3, T4, T5>(
            this Func<T1, T2, T3, T4, T5, Task> action) =>
            async (v1, v2, v3, v4, v5) =>
            {
                await action(v1, v2, v3, v4, v5);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, Task<Unit>> ToAsyncFunc<T1, T2, T3, T4, T5, T6>(
            this Func<T1, T2, T3, T4, T5, T6, Task> action) =>
            async (v1, v2, v3, v4, v5, v6) =>
            {
                await action(v1, v2, v3, v4, v5, v6);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, Task<Unit>> ToAsyncFunc<T1, T2, T3, T4, T5, T6, T7>(
            this Func<T1, T2, T3, T4, T5, T6, T7, Task> action) =>
            async (v1, v2, v3, v4, v5, v6, v7) =>
            {
                await action(v1, v2, v3, v4, v5, v6, v7);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, Task<Unit>> ToAsyncFunc<T1, T2, T3, T4, T5, T6, T7, T8>(
            this Func<T1, T2, T3, T4, T5, T6, T7, T8, Task> action) =>
            async (v1, v2, v3, v4, v5, v6, v7, v8) =>
            {
                await action(v1, v2, v3, v4, v5, v6, v7, v8);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, Task<Unit>> ToAsyncFunc<T1, T2, T3, T4, T5, T6, T7, T8,
            T9>(this Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, Task> action) =>
            async (v1, v2, v3, v4, v5, v6, v7, v8, v9) =>
            {
                await action(v1, v2, v3, v4, v5, v6, v7, v8, v9);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, Task<Unit>> ToAsyncFunc<T1, T2, T3, T4, T5, T6, T7,
            T8, T9, T10>(this Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, Task> action) =>
            async (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) =>
            {
                await action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, Task<Unit>> ToAsyncFunc<T1, T2, T3, T4, T5, T6,
            T7, T8, T9, T10, T11>(this Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, Task> action) =>
            async (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) =>
            {
                await action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, Task<Unit>> ToAsyncFunc<T1, T2, T3, T4,
            T5, T6, T7, T8, T9, T10, T11, T12>(
            this Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, Task> action) =>
            async (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) =>
            {
                await action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, Task<Unit>> ToAsyncFunc<T1, T2, T3,
            T4, T5, T6, T7, T8, T9, T10, T11, T12, T13>(
            this Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, Task> action) =>
            async (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) =>
            {
                await action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, Task<Unit>> ToAsyncFunc<T1, T2,
            T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14>(
            this Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, Task> action) =>
            async (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) =>
            {
                await action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Task<Unit>> ToAsyncFunc<T1,
            T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15>(
            this Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Task> action) =>
            async (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) =>
            {
                await action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15);
                return Unit.Value;
            };

        public static Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, Task<Unit>>
            ToAsyncFunc<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16>(
                this Func<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, Task> action) =>
            async (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) =>
            {
                await action(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16);
                return Unit.Value;
            };
    }
}