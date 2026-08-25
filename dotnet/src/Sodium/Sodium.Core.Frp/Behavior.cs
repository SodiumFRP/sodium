using System;

namespace Sodium.Frp
{
    internal static class BehaviorInternal
    {
        internal static Behavior<T> ConstantImpl<T>(T value) => new Behavior<T>(value);

        internal static Behavior<T> ConstantLazyImpl<T>(Lazy<T> value) =>
            TransactionInternal.Apply((trans, _) => StreamInternal.NeverImpl<T>().HoldLazyInternal(trans, value));

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

        // Captures nothing but this behavior, so it is built once rather than on every firing.
        // Only the stream-backed constructor needs it; a constant behavior never updates.
        private readonly Action applyValueUpdate;

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

            // Assigned before Listen, because listening can replay firings already made in this
            // transaction and so run the handler below before the constructor returns.
            this.applyValueUpdate = this.ApplyValueUpdate;

            this.streamListener = TransactionInternal.Apply(
                (trans1, _) =>
                    this.stream.Listen(
                        Node<T>.Null,
                        trans1,
                        (trans2, a) =>
                        {
                            // Deliberately not MatchNone/MatchSome: those take callbacks, and this
                            // runs on every firing of every cell, so the closures they require were
                            // showing up as a large share of the cost of a single Send.
                            if (!this.valueUpdate.HasValue())
                            {
                                trans2.Last(this.applyValueUpdate);
                            }

                            this.valueUpdate = MaybeInternal.Some(a);
                        },
                        false));
        }

        private void ApplyValueUpdate()
        {
            if (this.valueUpdate.TryGetValue(out T v))
            {
                this.ValueProperty = v;
            }

            this.valueUpdate = MaybeInternal.None;
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

        internal T SampleImpl() => TransactionInternal.Apply((trans, _) => this.SampleNoTransaction());

        internal Lazy<T> SampleLazyImpl() => TransactionInternal.Apply((trans, _) => this.SampleLazy(trans));

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

        /// <summary>
        ///     The stream of this behavior's value: its current value, delivered in this transaction,
        ///     followed by every update.
        /// </summary>
        /// <remarks>
        ///     Both sources feed one output stream directly rather than going through a spark stream, a
        ///     snapshot of it and a merge - four streams where two will do. Value sits underneath
        ///     Cell.Listen, Apply and the switches, so it was a large part of what each of those cost.
        ///
        ///     The initial send is queued against a bare node of its own, exactly as the spark stream it
        ///     replaces was, and for the same reason: a fresh node ranks below everything, so the value
        ///     is delivered even when Value is called part-way through a drain. SwitchB does precisely
        ///     that - its handler builds a Value for the newly selected behavior mid-transaction - and
        ///     hanging the initial send off the output node instead makes the switch deliver a stale
        ///     value. The node costs nothing; it is the two intermediate streams that were expensive.
        ///
        ///     Coalescing right-wins leaves an update from this transaction in front of the initial
        ///     value, which is what merging with (left, right) =&gt; right used to do.
        /// </remarks>
        internal Stream<T> Value(TransactionInternal trans1)
        {
            Stream<T> @out = new Stream<T>(this.stream.KeepListenersAlive);

            trans1.Prioritized(new Node<UnitInternal>(), trans2 => @out.Send(trans2, this.SampleNoTransaction()));

            IListener l = this.Updates().Listen(@out.Node, trans1, (trans2, v) => @out.Send(trans2, v), false);

            return @out.UnsafeAttachListener(l).Coalesce(trans1, (left, right) => right);
        }

        internal Behavior<TResult> MapImpl<TResult>(Func<T, TResult> f) =>
            TransactionInternal.Apply(
                (trans, _) => this.Updates().MapImpl(f).HoldLazyInternal(trans, this.SampleLazy(trans).MapImpl(f)));

        // Lift is deliberately no longer built out of Apply. Chaining ApplyImpl once per extra
        // input made every input pay for a Value() - a spark stream, a snapshot, a merge and a
        // coalesce - so a six-way lift constructed around fifty streams and cost roughly 87KB.
        // This shape, which the IEnumerable overload in BehaviorExtensionMethods already used,
        // builds three streams whatever the arity: one pulse stream that every input feeds, a
        // coalesce collapsing a transaction's updates into a single firing, and a map that
        // recombines the inputs.
        //
        // Each input's new value is captured as it propagates rather than read back off the
        // behavior afterwards. A behavior applies its update through a listener on Node<T>.Null,
        // and the priority queue drains null-ranked entries only after every ranked one, so at the
        // point the map below runs the behaviors still hold their previous values. Rank ordering is
        // what makes capturing safe: every input links into pulse.Node, so pulse.Node outranks all
        // of them and each slot is filled before anything downstream of the coalesce can run.

        internal Behavior<TResult> LiftImpl<T2, TResult>(Behavior<T2> b2, Func<T, T2, TResult> f) =>
            TransactionInternal.Apply(
                (trans, _) =>
                {
                    Stream<UnitInternal> pulse = new Stream<UnitInternal>(this.stream.KeepListenersAlive);

                    MaybeInternal<T> p1 = MaybeInternal.None;
                    MaybeInternal<T2> p2 = MaybeInternal.None;

                    IListener[] listeners =
                    {
                        Pulse(this, pulse, trans, v => p1 = MaybeInternal.Some(v)),
                        Pulse(b2, pulse, trans, v => p2 = MaybeInternal.Some(v))
                    };

                    return HoldLifted(
                        pulse,
                        trans,
                        () =>
                        {
                            TResult result = f(Take(ref p1, this), Take(ref p2, b2));
                            return result;
                        },
                        () => f(this.SampleNoTransaction(), b2.SampleNoTransaction()),
                        listeners);
                });

        internal Behavior<TResult> LiftImpl<T2, T3, TResult>(Behavior<T2> b2, Behavior<T3> b3, Func<T, T2, T3, TResult> f) =>
            TransactionInternal.Apply(
                (trans, _) =>
                {
                    Stream<UnitInternal> pulse = new Stream<UnitInternal>(this.stream.KeepListenersAlive);

                    MaybeInternal<T> p1 = MaybeInternal.None;
                    MaybeInternal<T2> p2 = MaybeInternal.None;
                    MaybeInternal<T3> p3 = MaybeInternal.None;

                    IListener[] listeners =
                    {
                        Pulse(this, pulse, trans, v => p1 = MaybeInternal.Some(v)),
                        Pulse(b2, pulse, trans, v => p2 = MaybeInternal.Some(v)),
                        Pulse(b3, pulse, trans, v => p3 = MaybeInternal.Some(v))
                    };

                    return HoldLifted(
                        pulse,
                        trans,
                        () => f(Take(ref p1, this), Take(ref p2, b2), Take(ref p3, b3)),
                        () => f(this.SampleNoTransaction(), b2.SampleNoTransaction(), b3.SampleNoTransaction()),
                        listeners);
                });

        internal Behavior<TResult> LiftImpl<T2, T3, T4, TResult>(
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Func<T, T2, T3, T4, TResult> f) =>
            TransactionInternal.Apply(
                (trans, _) =>
                {
                    Stream<UnitInternal> pulse = new Stream<UnitInternal>(this.stream.KeepListenersAlive);

                    MaybeInternal<T> p1 = MaybeInternal.None;
                    MaybeInternal<T2> p2 = MaybeInternal.None;
                    MaybeInternal<T3> p3 = MaybeInternal.None;
                    MaybeInternal<T4> p4 = MaybeInternal.None;

                    IListener[] listeners =
                    {
                        Pulse(this, pulse, trans, v => p1 = MaybeInternal.Some(v)),
                        Pulse(b2, pulse, trans, v => p2 = MaybeInternal.Some(v)),
                        Pulse(b3, pulse, trans, v => p3 = MaybeInternal.Some(v)),
                        Pulse(b4, pulse, trans, v => p4 = MaybeInternal.Some(v))
                    };

                    return HoldLifted(
                        pulse,
                        trans,
                        () => f(Take(ref p1, this), Take(ref p2, b2), Take(ref p3, b3), Take(ref p4, b4)),
                        () => f(
                            this.SampleNoTransaction(),
                            b2.SampleNoTransaction(),
                            b3.SampleNoTransaction(),
                            b4.SampleNoTransaction()),
                        listeners);
                });

        internal Behavior<TResult> LiftImpl<T2, T3, T4, T5, TResult>(
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Behavior<T5> b5,
            Func<T, T2, T3, T4, T5, TResult> f) =>
            TransactionInternal.Apply(
                (trans, _) =>
                {
                    Stream<UnitInternal> pulse = new Stream<UnitInternal>(this.stream.KeepListenersAlive);

                    MaybeInternal<T> p1 = MaybeInternal.None;
                    MaybeInternal<T2> p2 = MaybeInternal.None;
                    MaybeInternal<T3> p3 = MaybeInternal.None;
                    MaybeInternal<T4> p4 = MaybeInternal.None;
                    MaybeInternal<T5> p5 = MaybeInternal.None;

                    IListener[] listeners =
                    {
                        Pulse(this, pulse, trans, v => p1 = MaybeInternal.Some(v)),
                        Pulse(b2, pulse, trans, v => p2 = MaybeInternal.Some(v)),
                        Pulse(b3, pulse, trans, v => p3 = MaybeInternal.Some(v)),
                        Pulse(b4, pulse, trans, v => p4 = MaybeInternal.Some(v)),
                        Pulse(b5, pulse, trans, v => p5 = MaybeInternal.Some(v))
                    };

                    return HoldLifted(
                        pulse,
                        trans,
                        () => f(Take(ref p1, this), Take(ref p2, b2), Take(ref p3, b3), Take(ref p4, b4), Take(ref p5, b5)),
                        () => f(
                            this.SampleNoTransaction(),
                            b2.SampleNoTransaction(),
                            b3.SampleNoTransaction(),
                            b4.SampleNoTransaction(),
                            b5.SampleNoTransaction()),
                        listeners);
                });

        internal Behavior<TResult> LiftImpl<T2, T3, T4, T5, T6, TResult>(
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Behavior<T5> b5,
            Behavior<T6> b6,
            Func<T, T2, T3, T4, T5, T6, TResult> f) =>
            TransactionInternal.Apply(
                (trans, _) =>
                {
                    Stream<UnitInternal> pulse = new Stream<UnitInternal>(this.stream.KeepListenersAlive);

                    MaybeInternal<T> p1 = MaybeInternal.None;
                    MaybeInternal<T2> p2 = MaybeInternal.None;
                    MaybeInternal<T3> p3 = MaybeInternal.None;
                    MaybeInternal<T4> p4 = MaybeInternal.None;
                    MaybeInternal<T5> p5 = MaybeInternal.None;
                    MaybeInternal<T6> p6 = MaybeInternal.None;

                    IListener[] listeners =
                    {
                        Pulse(this, pulse, trans, v => p1 = MaybeInternal.Some(v)),
                        Pulse(b2, pulse, trans, v => p2 = MaybeInternal.Some(v)),
                        Pulse(b3, pulse, trans, v => p3 = MaybeInternal.Some(v)),
                        Pulse(b4, pulse, trans, v => p4 = MaybeInternal.Some(v)),
                        Pulse(b5, pulse, trans, v => p5 = MaybeInternal.Some(v)),
                        Pulse(b6, pulse, trans, v => p6 = MaybeInternal.Some(v))
                    };

                    return HoldLifted(
                        pulse,
                        trans,
                        () => f(
                            Take(ref p1, this),
                            Take(ref p2, b2),
                            Take(ref p3, b3),
                            Take(ref p4, b4),
                            Take(ref p5, b5),
                            Take(ref p6, b6)),
                        () => f(
                            this.SampleNoTransaction(),
                            b2.SampleNoTransaction(),
                            b3.SampleNoTransaction(),
                            b4.SampleNoTransaction(),
                            b5.SampleNoTransaction(),
                            b6.SampleNoTransaction()),
                        listeners);
                });

        /// <summary>
        ///     Wires one lifted input to the shared pulse stream, recording its new value on the way
        ///     through so the recombine step does not have to read it back off the behavior.
        /// </summary>
        private static IListener Pulse<TInput>(
            Behavior<TInput> input,
            Stream<UnitInternal> pulse,
            TransactionInternal trans,
            Action<TInput> capture) =>
            input.Updates()
                .Listen(
                    pulse.Node,
                    trans,
                    (trans2, v) =>
                    {
                        capture(v);
                        pulse.Send(trans2, UnitInternal.Value);
                    },
                    false);

        /// <summary>
        ///     Reads an input's value for this firing: the value captured on the way through if it
        ///     updated in this transaction, otherwise the behavior's current one.
        /// </summary>
        /// <remarks>
        ///     Clearing the slot afterwards is hygiene rather than correctness. A slot left set would
        ///     still give the right answer, because by the time the next transaction reads it the
        ///     behavior has committed that same value - verified by removing the reset and finding no
        ///     test could tell. It is cleared so the closure does not hold a second reference to every
        ///     input's last value for as long as the lifted behavior lives.
        /// </remarks>
        private static TInput Take<TInput>(ref MaybeInternal<TInput> pending, Behavior<TInput> input)
        {
            TInput value = pending.TryGetValue(out TInput captured) ? captured : input.SampleNoTransaction();
            pending = MaybeInternal.None;
            return value;
        }

        private static Behavior<TResult> HoldLifted<TResult>(
            Stream<UnitInternal> pulse,
            TransactionInternal trans,
            Func<TResult> recombine,
            Func<TResult> initialValue,
            IListener[] listeners)
        {
            // Coalescing means a transaction that updates several inputs produces exactly one
            // firing, with every input's new value already captured.
            Stream<TResult> result = pulse.Coalesce(trans, (x, y) => x).MapImpl(_ => recombine());

            foreach (IListener listener in listeners)
            {
                result = result.UnsafeAttachListener(listener);
            }

            return result.HoldLazyInternal(trans, new Lazy<TResult>(initialValue));
        }

        internal Behavior<TResult> ApplyImpl<TResult>(Behavior<Func<T, TResult>> bf)
        {
            return TransactionInternal.Apply(
                (trans0, _) =>
                {
                    Stream<TResult> @out = new Stream<TResult>(this.stream.KeepListenersAlive);

                    Node<TResult> outTarget = @out.Node;
                    Node<UnitInternal> inTarget = new Node<UnitInternal>();
                    Node<UnitInternal>.Target nodeTarget = inTarget.Link(trans0, (t, v) => { }, outTarget);

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
                });
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