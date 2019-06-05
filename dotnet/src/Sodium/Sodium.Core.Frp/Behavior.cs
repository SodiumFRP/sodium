using System;

namespace Sodium.Frp
{
    internal static class BehaviorInternal
    {
        internal static Behavior<T> ConstantImpl<T>(T value) => new Behavior<T>(value);

        internal static Behavior<T> ConstantLazyImpl<T>(Lazy<T> value) =>
            TransactionInternal.Apply((trans, _) => StreamInternal.NeverImpl<T>().HoldLazyInternal(trans, value), false);

        internal static BehaviorSink<T> CreateSinkImpl<T>(T initialValue) => new BehaviorSink<T>(initialValue);

        internal static BehaviorSink<T> CreateSinkImpl<T>(T initialValue, Func<T, T, T> coalesce) =>
            new BehaviorSink<T>(initialValue, coalesce);
    }

    /// <summary>
    ///     Represents a value that changes over time.
    /// </summary>
    /// <typeparam name="T">The type of values in the behavior.</typeparam>
    public class Behavior<T>
    {
        private readonly Stream<T> stream;
        private MaybeInternal<T> valueUpdate;

        // ReSharper disable once NotAccessedField.Local - Used to keep object from being garbage collected
        private readonly IListener streamListener;

        private T valueProperty;

        internal Behavior(T value)
        {
            this.stream = new Stream<T>();
            this.ValueProperty = value;
        }

        internal Behavior(Stream<T> stream, T initialValue)
        {
            this.stream = stream;
            this.valueProperty = initialValue;
            this.UsingInitialValue = true;

            this.streamListener = TransactionInternal.Apply(
                (trans1, _) =>
                    this.stream.Listen(
                        Node<T>.Null,
                        trans1,
                        (trans2, a) =>
                        {
                            this.valueUpdate.MatchNone(
                                () =>
                                {
                                    trans2.Last(
                                        () =>
                                        {
                                            this.valueUpdate.MatchSome(v => this.ValueProperty = v);
                                            this.valueUpdate = MaybeInternal.None;
                                        });
                                });

                            this.valueUpdate = MaybeInternal.Some(a);
                        },
                        false),
                false);
        }

        internal IKeepListenersAlive KeepListenersAlive => this.stream.KeepListenersAlive;

        protected T ValueProperty
        {
            get => this.valueProperty;
            set
            {
                this.valueProperty = value;
                this.NotUsingInitialValue();
            }
        }

        protected virtual void NotUsingInitialValue()
        {
            this.UsingInitialValue = false;
        }

        protected bool UsingInitialValue { get; private set; }

        internal T SampleImpl() => TransactionInternal.Apply((trans, _) => this.SampleNoTransaction(), true);

        internal Lazy<T> SampleLazyImpl() => TransactionInternal.Apply((trans, _) => this.SampleLazy(trans), false);

        internal Lazy<T> SampleLazy(TransactionInternal trans)
        {
            LazySample s = new LazySample(this);
            trans.Sample(
                () =>
                {
                    s.Value = this.valueUpdate.Match(v => v, this.SampleNoTransaction);
                    s.HasValue = true;
                    s.Behavior = null;
                });
            return new Lazy<T>(() => s.HasValue ? s.Value : s.Behavior.SampleImpl());
        }

        internal virtual T SampleNoTransaction() => this.ValueProperty;

        internal Stream<T> Updates() => this.stream;

        internal Stream<T> Value(TransactionInternal trans1)
        {
            Stream<UnitInternal> spark = new Stream<UnitInternal>(this.stream.KeepListenersAlive);
            trans1.Prioritized(spark.Node, trans2 => spark.Send(trans2, UnitInternal.Value));
            Stream<T> initial = spark.SnapshotImpl(this);
            return initial.Merge(trans1, this.Updates(), (left, right) => right);
        }

        internal Behavior<TResult> MapImpl<TResult>(Func<T, TResult> f) =>
            TransactionInternal.Apply(
                (trans, _) => this.Updates().MapImpl(f).HoldLazyInternal(trans, this.SampleLazy(trans).MapImpl(f)),
                false);

        internal Behavior<TResult> LiftImpl<T2, TResult>(Behavior<T2> b2, Func<T, T2, TResult> f)
        {
            Func<T2, TResult> Ffa(T a) => b => f(a, b);
            return b2.ApplyImpl(this.MapImpl(Ffa));
        }

        internal Behavior<TResult> LiftImpl<T2, T3, TResult>(Behavior<T2> b2, Behavior<T3> b3, Func<T, T2, T3, TResult> f)
        {
            Func<T2, Func<T3, TResult>> Ffa(T a) => b => c => f(a, b, c);
            return b3.ApplyImpl(b2.ApplyImpl(this.MapImpl(Ffa)));
        }

        internal Behavior<TResult> LiftImpl<T2, T3, T4, TResult>(
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Func<T, T2, T3, T4, TResult> f)
        {
            Func<T2, Func<T3, Func<T4, TResult>>> Ffa(T a) => b => c => d => f(a, b, c, d);
            return b4.ApplyImpl(b3.ApplyImpl(b2.ApplyImpl(this.MapImpl(Ffa))));
        }

        internal Behavior<TResult> LiftImpl<T2, T3, T4, T5, TResult>(
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Behavior<T5> b5,
            Func<T, T2, T3, T4, T5, TResult> f)
        {
            Func<T2, Func<T3, Func<T4, Func<T5, TResult>>>> Ffa(T a) => b => c => d => e => f(a, b, c, d, e);
            return b5.ApplyImpl(b4.ApplyImpl(b3.ApplyImpl(b2.ApplyImpl(this.MapImpl(Ffa)))));
        }

        internal Behavior<TResult> LiftImpl<T2, T3, T4, T5, T6, TResult>(
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Behavior<T5> b5,
            Behavior<T6> b6,
            Func<T, T2, T3, T4, T5, T6, TResult> f)
        {
            Func<T2, Func<T3, Func<T4, Func<T5, Func<T6, TResult>>>>> Ffa(T a) =>
                b => c => d => e => ff => f(a, b, c, d, e, ff);

            return b6.ApplyImpl(b5.ApplyImpl(b4.ApplyImpl(b3.ApplyImpl(b2.ApplyImpl(this.MapImpl(Ffa))))));
        }

        internal Behavior<TResult> ApplyImpl<TResult>(Behavior<Func<T, TResult>> bf)
        {
            return TransactionInternal.Apply(
                (trans0, _) =>
                {
                    Stream<TResult> @out = new Stream<TResult>(this.stream.KeepListenersAlive);

                    Node<TResult> outTarget = @out.Node;
                    Node<UnitInternal> inTarget = new Node<UnitInternal>();
                    (bool changed, Node<UnitInternal>.Target nodeTarget) = inTarget.Link(trans0, (t, v) => { }, outTarget);
                    if (changed)
                    {
                        trans0.SetNeedsRegenerating();
                    }

                    Func<T, TResult> f = null;
                    T a = default(T);
                    bool isASet = false;

                    // ReSharper disable once PossibleNullReferenceException
                    void H(TransactionInternal trans1) => trans1.Prioritized(@out.Node, trans2 => @out.Send(trans2, f(a)));

                    IListener l1 = bf.Value(trans0)
                        .Listen(
                            inTarget,
                            trans0,
                            (trans1, ff) =>
                            {
                                f = ff;
                                if (isASet)
                                {
                                    H(trans1);
                                }
                            },
                            false);
                    IListener l2 = this.Value(trans0)
                        .Listen(
                            inTarget,
                            trans0,
                            (trans1, aa) =>
                            {
                                a = aa;
                                isASet = true;
                                if (f != null)
                                {
                                    H(trans1);
                                }
                            },
                            false);
                    return @out.LastFiringOnly(trans0)
                        .UnsafeAttachListener(l1)
                        .UnsafeAttachListener(l2)
                        .UnsafeAttachListener(
                            ListenerInternal.CreateFromNodeAndTarget(inTarget, nodeTarget))
                        .HoldLazyInternal(
                            trans0,
                            new Lazy<TResult>(() => bf.SampleNoTransaction()(this.SampleNoTransaction())));
                },
                false);
        }

        private class LazySample
        {
            internal Behavior<T> Behavior;
            internal bool HasValue;
            internal T Value;

            internal LazySample(Behavior<T> behavior) => this.Behavior = behavior;
        }
    }
}