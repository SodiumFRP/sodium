using System;

namespace Sodium.Frp
{
    internal static class CellInternal
    {
        internal static Cell<T> ConstantImpl<T>(T value) =>
            new Cell<T>(StreamInternal.NeverImpl<T>().HoldInternal(value));

        internal static Cell<T> ConstantLazyImpl<T>(Lazy<T> value) =>
            TransactionInternal.Apply((trans, _) => new Cell<T>(StreamInternal.NeverImpl<T>().HoldLazyInternal(trans, value)), false);

        internal static CellSink<T> CreateSinkImpl<T>(T initialValue) => new CellSink<T>(initialValue);

        internal static CellSink<T> CreateSinkImpl<T>(T initialValue, Func<T, T, T> coalesce) => new CellSink<T>(initialValue, coalesce);

        internal static CellStreamSink<T> CreateStreamSinkImpl<T>() => new CellStreamSink<T>();

        internal static CellStreamSink<T> CreateStreamSinkImpl<T>(Func<T, T, T> coalesce) =>
            new CellStreamSink<T>(coalesce);
    }

    /// <summary>
    ///     Represents a value that discretely changes over time.
    /// </summary>
    /// <typeparam name="T">The type of the value.</typeparam>
    public class Cell<T>
    {
        private readonly Lazy<Stream<T>> updates;

        internal Cell(Behavior<T> behavior)
        {
            this.BehaviorImpl = behavior;

            this.updates = new Lazy<Stream<T>>(() => TransactionInternal.Apply(
                (trans, _) => this.BehaviorImpl.Updates().Coalesce(trans, (left, right) => right),
                false));
        }

        internal T SampleImpl() => this.BehaviorImpl.SampleImpl();

        internal Lazy<T> SampleLazyImpl() => this.BehaviorImpl.SampleLazyImpl();

        internal Stream<T> UpdatesImpl => this.updates.Value;

        internal Stream<T> ValuesImpl => TransactionInternal.Apply((trans, _) => this.BehaviorImpl.Value(trans), false);

        internal Behavior<T> BehaviorImpl { get; }

        internal IStrongListener ListenImpl(Action<T> handler) => TransactionInternal.Apply(
            (trans, _) => this.BehaviorImpl.Value(trans).ListenImpl(handler),
            false);

        internal IWeakListener ListenWeakImpl(Action<T> handler) => TransactionInternal.Apply(
            (trans, _) => this.BehaviorImpl.Value(trans).ListenWeakImpl(handler),
            false);

        internal Cell<TResult> MapImpl<TResult>(Func<T, TResult> f) =>
            new Cell<TResult>(this.BehaviorImpl.MapImpl(f));

        internal Cell<TResult> LiftImpl<T2, TResult>(Cell<T2> b2, Func<T, T2, TResult> f) =>
            new Cell<TResult>(this.BehaviorImpl.LiftImpl(b2.BehaviorImpl, f));

        internal Cell<TResult> LiftImpl<T2, T3, TResult>(
            Cell<T2> b2,
            Cell<T3> b3,
            Func<T, T2, T3, TResult> f) =>
            new Cell<TResult>(this.BehaviorImpl.LiftImpl(b2.BehaviorImpl, b3.BehaviorImpl, f));

        internal Cell<TResult> LiftImpl<T2, T3, T4, TResult>(
            Cell<T2> b2,
            Cell<T3> b3,
            Cell<T4> b4,
            Func<T, T2, T3, T4, TResult> f) =>
            new Cell<TResult>(this.BehaviorImpl.LiftImpl(b2.BehaviorImpl, b3.BehaviorImpl, b4.BehaviorImpl, f));

        internal Cell<TResult> LiftImpl<T2, T3, T4, T5, TResult>(
            Cell<T2> b2,
            Cell<T3> b3,
            Cell<T4> b4,
            Cell<T5> b5,
            Func<T, T2, T3, T4, T5, TResult> f) =>
            new Cell<TResult>(this.BehaviorImpl.LiftImpl(b2.BehaviorImpl, b3.BehaviorImpl, b4.BehaviorImpl, b5.BehaviorImpl, f));

        internal Cell<TResult> LiftImpl<T2, T3, T4, T5, T6, TResult>(
            Cell<T2> b2,
            Cell<T3> b3,
            Cell<T4> b4,
            Cell<T5> b5,
            Cell<T6> b6,
            Func<T, T2, T3, T4, T5, T6, TResult> f) =>
            new Cell<TResult>(this.BehaviorImpl.LiftImpl(b2.BehaviorImpl, b3.BehaviorImpl, b4.BehaviorImpl, b5.BehaviorImpl, b6.BehaviorImpl, f));

        internal Cell<TResult> ApplyImpl<TResult>(Cell<Func<T, TResult>> bf) =>
            new Cell<TResult>(this.BehaviorImpl.ApplyImpl(bf.BehaviorImpl));

        internal Cell<T> CalmImpl(Func<T, T, bool> areEqual)
        {
            Lazy<T> initA = this.BehaviorImpl.SampleLazyImpl();
            Lazy<MaybeInternal<T>> mInitA = initA.MapImpl(MaybeInternal.Some);
            return this.UpdatesImpl.Calm(mInitA, areEqual).HoldLazyImpl(initA);
        }
    }
}