using System;
using JetBrains.Annotations;

namespace Sodium
{
    /// <summary>
    ///     Helper methods for creating a <see cref="Behavior{T}" />.
    /// </summary>
    public static class Behavior
    {
        /// <summary>
        ///     Creates a behavior with a constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the behavior.</typeparam>
        /// <param name="value">The value of the behavior.</param>
        /// <returns>A behavior with a constant value.</returns>
        public static Behavior<T> Constant<T>(T value) => new Behavior<T>(value);

        /// <summary>
        ///     Creates a behavior with a lazily computed constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the behavior.</typeparam>
        /// <param name="value">The lazily computed value of the behavior.</param>
        /// <returns>A behavior with a lazily computed constant value.</returns>
        public static Behavior<T> ConstantLazy<T>(Lazy<T> value) =>
            Transaction.Apply(trans => Stream.Never<T>().HoldLazyInternal(trans, value), false);

        /// <summary>
        ///     Creates a writable behavior that uses the last value if <see cref="BehaviorSink{T}.Send" /> is called more than once per
        ///     transaction.
        /// </summary>
        /// <param name="initialValue">The initial value of the behavior.</param>
        /// <typeparam name="T">The type of values in the behavior sink.</typeparam>
        public static BehaviorSink<T> CreateSink<T>(T initialValue) => new BehaviorSink<T>(initialValue);

        /// <summary>
        ///     Creates a writable behavior that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="BehaviorSink{T}.Send" /> is called more than once per transaction.
        /// </summary>
        /// <param name="initialValue">The initial value of the behavior.</param>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="BehaviorSink{T}.Send(T)" /> is called more than once per
        ///     transaction.
        /// </param>
        /// <typeparam name="T">The type of values in the behavior sink.</typeparam>
        public static BehaviorSink<T> CreateSink<T>(T initialValue, Func<T, T, T> coalesce) =>
            new BehaviorSink<T>(initialValue, coalesce);

        /// <summary>
        ///     Creates a <see cref="BehaviorLoop{T}" />.  This must be called and looped from within the same transaction.
        /// </summary>
        /// <typeparam name="T">The type of values in the behavior loop.</typeparam>
        public static BehaviorLoop<T> CreateLoop<T>() => new BehaviorLoop<T>();

        /// <summary>
        ///     Creates a helper to loop over a behavior for the specified type.
        /// </summary>
        /// <typeparam name="T">The type of the behavior to loop.</typeparam>
        /// <returns>A <see cref="BehaviorLooper{T}"/> which should be used to complete the loop.</returns>
        [Pure]
        public static BehaviorLooper<T> Loop<T>() => new BehaviorLooper<T>();
    }

    /// <summary>
    ///     A helper to complete a loop over a behavior.
    /// </summary>
    /// <typeparam name="T">The type of the behavior being looped.</typeparam>
    public struct BehaviorLooper<T>
    {
        /// <summary>
        ///     Loop a behavior and return a value tuple containing the resulting behavior and captures.
        /// </summary>
        /// <typeparam name="TCaptures">The type of the captures to return.</typeparam>
        /// <param name="f">A function which takes the behavior loop and returns a value tuple containing the resulting behavior and captures.</param>
        /// <returns>A value tuple containing the resulting behavior and captures.</returns>
        [Pure]
        public (Behavior<T> Behavior, TCaptures Captures) WithCaptures<TCaptures>(
            Func<LoopedBehavior<T>, (Behavior<T> Behavior, TCaptures Captures)> f) =>
            Transaction.Apply(
                trans =>
                {
                    LoopedBehavior<T> loop = new LoopedBehavior<T>();
                    (Behavior<T> Behavior, TCaptures Captures) result = f(loop);
                    loop.Loop(trans, result.Behavior);
                    return result;
                },
                false);

        /// <summary>
        ///     Loop a behavior and return the resulting behavior.
        /// </summary>
        /// <param name="f">A function which takes the behavior loop and returns the resulting behavior.</param>
        /// <returns>The resulting behavior.</returns>
        [Pure]
        public Behavior<T> WithoutCaptures(Func<LoopedBehavior<T>, Behavior<T>> f) =>
            this.WithCaptures(l => (Behavior: f(l), Captures: Unit.Value)).Behavior;
    }

    /// <summary>
    ///     Represents a value that changes over time.
    /// </summary>
    /// <typeparam name="T">The type of values in the behavior.</typeparam>
    public class Behavior<T>
    {
        private readonly Stream<T> stream;
        private Maybe<T> valueUpdate;

        // ReSharper disable once NotAccessedField.Local - Used to keep object from being garbage collected
        private readonly IListener streamListener;

        private T valueProperty;

        /// <summary>
        ///     Creates a behavior with a constant value.
        /// </summary>
        /// <param name="value">The constant value of the behavior.</param>
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

            this.streamListener = Transaction.Apply(
                trans1 =>
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
                                            this.valueUpdate = Maybe.None;
                                        });
                                });

                            this.valueUpdate = Maybe.Some(a);
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

        /// <summary>
        ///     Sample the current value of the behavior.
        /// </summary>
        /// <returns>The current value of the behavior.</returns>
        /// <remarks>
        ///     <para>
        ///         This method may be used inside the functions passed to primitives that apply them to streams,
        ///         including <see cref="Stream{T}.Map{TResult}(Func{T, TResult})" /> in which case it is equivalent to
        ///         snapshotting the behavior,
        ///         <see cref=" Stream{T}.Snapshot{T2, TResult}(Behavior{T2}, Func{T, T2, TResult})" />,
        ///         <see cref="Stream{T}.Filter(Func{T, bool})" />, and
        ///         <see cref="Stream{T}.Merge(Stream{T}, Func{T, T, T})" />
        ///     </para>
        ///     <para>
        ///         It can be best to use this method inside an explicit transaction (using
        ///         <see cref="Transaction.Run{T}(Func{T})" /> or <see cref="Transaction.RunVoid(Action)" />).
        ///         For example, a b.Sample() inside an explicit transaction along with a b.Updates().Listen(...) will capture the
        ///         current value and any updates without risk of missing any in between.
        ///     </para>
        /// </remarks>
        public T Sample() => Transaction.Apply(trans => this.SampleNoTransaction(), true);

        /// <summary>
        ///     Sample the current value of the behavior lazily.
        /// </summary>
        /// <returns>A lazy which may be used to get the current value of the behavior.</returns>
        /// <remarks>
        ///     This is a variant of <see cref="Sample" /> that works with the <see cref="BehaviorLoop{T}" /> class
        ///     when the behavior loop has not yet been looped.  It should be used in any code that is general
        ///     enough that it may be passed a <see cref="BehaviorLoop{T}" />.  See <see cref="Stream{T}.HoldLazy(Lazy{T})" />.
        /// </remarks>
        public Lazy<T> SampleLazy() => Transaction.Apply(this.SampleLazy, false);

        internal Lazy<T> SampleLazy(Transaction trans)
        {
            LazySample s = new LazySample(this);
            trans.Sample(
                () =>
                {
                    s.Value = this.valueUpdate.Match(v => v, this.SampleNoTransaction);
                    s.HasValue = true;
                    s.Behavior = null;
                });
            return new Lazy<T>(() => s.HasValue ? s.Value : s.Behavior.Sample());
        }

        internal virtual T SampleNoTransaction() => this.ValueProperty;

        internal Stream<T> Updates() => this.stream;

        internal Stream<T> Value(Transaction trans1)
        {
            Stream<Unit> spark = new Stream<Unit>(this.stream.KeepListenersAlive);
            trans1.Prioritized(spark.Node, trans2 => spark.Send(trans2, Unit.Value));
            Stream<T> initial = spark.Snapshot(this);
            return initial.Merge(trans1, this.Updates(), (left, right) => right);
        }

        /// <summary>
        ///     Transform the behavior values according to the supplied function, so the returned
        ///     behavior's values reflect the value of the function applied to the input behavior's values.
        /// </summary>
        /// <typeparam name="TResult">The type of values fired by the returned behavior.</typeparam>
        /// <param name="f">
        ///     Function to apply to convert the values.  It must be a pure function.
        /// </param>
        /// <returns>An behavior which fires values transformed by <paramref name="f" /> for each value fired by this behavior.</returns>
        public Behavior<TResult> Map<TResult>(Func<T, TResult> f)
        {
            return Transaction.Apply(
                trans => this.Updates().Map(f).HoldLazyInternal(trans, this.SampleLazy(trans).Map(f)),
                false);
        }

        /// <summary>
        ///     Lift a binary function into behaviors, so the returned behavior always reflects the specified function applied to the input
        ///     behaviors' values.
        /// </summary>
        /// <typeparam name="T2">The type of second behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <param name="b2">The second behavior.</param>
        /// <returns>A behavior containing values resulting from the binary function applied to the input behaviors' values.</returns>
        public Behavior<TResult> Lift<T2, TResult>(Behavior<T2> b2, Func<T, T2, TResult> f)
        {
            Func<T2, TResult> Ffa(T a) => b => f(a, b);
            return b2.Apply(this.Map(Ffa));
        }

        /// <summary>
        ///     Lift a ternary function into behaviors, so the returned behavior always reflects the specified function applied to the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T2">The type of second behavior.</typeparam>
        /// <typeparam name="T3">The type of third behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <param name="b2">The second behavior.</param>
        /// <param name="b3">The third behavior.</param>
        /// <returns>A behavior containing values resulting from the ternary function applied to the input behaviors' values.</returns>
        public Behavior<TResult> Lift<T2, T3, TResult>(Behavior<T2> b2, Behavior<T3> b3, Func<T, T2, T3, TResult> f)
        {
            Func<T2, Func<T3, TResult>> Ffa(T a) => b => c => f(a, b, c);
            return b3.Apply(b2.Apply(this.Map(Ffa)));
        }

        /// <summary>
        ///     Lift a quaternary function into behaviors, so the returned behavior always reflects the specified function applied to the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T2">The type of second behavior.</typeparam>
        /// <typeparam name="T3">The type of third behavior.</typeparam>
        /// <typeparam name="T4">The type of fourth behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <param name="b2">The second behavior.</param>
        /// <param name="b3">The third behavior.</param>
        /// <param name="b4">The fourth behavior.</param>
        /// <returns>A behavior containing values resulting from the quaternary function applied to the input behaviors' values.</returns>
        public Behavior<TResult> Lift<T2, T3, T4, TResult>(
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Func<T, T2, T3, T4, TResult> f)
        {
            Func<T2, Func<T3, Func<T4, TResult>>> Ffa(T a) => b => c => d => f(a, b, c, d);
            return b4.Apply(b3.Apply(b2.Apply(this.Map(Ffa))));
        }

        /// <summary>
        ///     Lift a 5-argument function into behaviors, so the returned behavior always reflects the specified function applied to the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T2">The type of second behavior.</typeparam>
        /// <typeparam name="T3">The type of third behavior.</typeparam>
        /// <typeparam name="T4">The type of fourth behavior.</typeparam>
        /// <typeparam name="T5">The type of fifth behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <param name="b2">The second behavior.</param>
        /// <param name="b3">The third behavior.</param>
        /// <param name="b4">The fourth behavior.</param>
        /// <param name="b5">The fifth behavior.</param>
        /// <returns>A behavior containing values resulting from the 5-argument function applied to the input behaviors' values.</returns>
        public Behavior<TResult> Lift<T2, T3, T4, T5, TResult>(
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Behavior<T5> b5,
            Func<T, T2, T3, T4, T5, TResult> f)
        {
            Func<T2, Func<T3, Func<T4, Func<T5, TResult>>>> Ffa(T a) => b => c => d => e => f(a, b, c, d, e);
            return b5.Apply(b4.Apply(b3.Apply(b2.Apply(this.Map(Ffa)))));
        }

        /// <summary>
        ///     Lift a 6-argument function into behaviors, so the returned behavior always reflects the specified function applied to the
        ///     input behaviors' values.
        /// </summary>
        /// <typeparam name="T2">The type of second behavior.</typeparam>
        /// <typeparam name="T3">The type of third behavior.</typeparam>
        /// <typeparam name="T4">The type of fourth behavior.</typeparam>
        /// <typeparam name="T5">The type of fifth behavior.</typeparam>
        /// <typeparam name="T6">The type of sixth behavior.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="f">The binary function to lift into the behaviors.</param>
        /// <param name="b2">The second behavior.</param>
        /// <param name="b3">The third behavior.</param>
        /// <param name="b4">The fourth behavior.</param>
        /// <param name="b5">The fifth behavior.</param>
        /// <param name="b6">The sixth behavior.</param>
        /// <returns>A behavior containing values resulting from the 6-argument function applied to the input behaviors' values.</returns>
        public Behavior<TResult> Lift<T2, T3, T4, T5, T6, TResult>(
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Behavior<T5> b5,
            Behavior<T6> b6,
            Func<T, T2, T3, T4, T5, T6, TResult> f)
        {
            Func<T2, Func<T3, Func<T4, Func<T5, Func<T6, TResult>>>>> Ffa(T a) =>
                b => c => d => e => ff => f(a, b, c, d, e, ff);

            return b6.Apply(b5.Apply(b4.Apply(b3.Apply(b2.Apply(this.Map(Ffa))))));
        }

        /// <summary>
        ///     Apply a value inside a behavior to a function inside a behavior.  This is the primitive for all function lifting.
        /// </summary>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="bf">The behavior containing the function to apply the value to.</param>
        /// <returns>
        ///     A behavior whose value is the result of applying the current function in behavior <paramref name="bf" /> to this
        ///     behavior's current value.
        /// </returns>
        public Behavior<TResult> Apply<TResult>(Behavior<Func<T, TResult>> bf)
        {
            return Transaction.Apply(
                trans0 =>
                {
                    Stream<TResult> @out = new Stream<TResult>(this.stream.KeepListenersAlive);

                    Node<TResult> outTarget = @out.Node;
                    Node<Unit> inTarget = new Node<Unit>();
                    (bool changed, Node<Unit>.Target nodeTarget) = inTarget.Link(trans0, (t, v) => { }, outTarget);
                    if (changed)
                    {
                        trans0.SetNeedsRegenerating();
                    }

                    Func<T, TResult> f = null;
                    T a = default(T);
                    bool isASet = false;

                    // ReSharper disable once PossibleNullReferenceException
                    void H(Transaction trans1) => trans1.Prioritized(@out.Node, trans2 => @out.Send(trans2, f(a)));

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
                            Listener.Create(inTarget, nodeTarget))
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