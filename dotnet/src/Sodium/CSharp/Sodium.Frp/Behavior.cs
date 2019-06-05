using System;
using System.Runtime.CompilerServices;

namespace Sodium.Frp
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
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<T> Constant<T>(T value) => BehaviorInternal.ConstantImpl(value);

        /// <summary>
        ///     Creates a behavior with a lazily computed constant value.
        /// </summary>
        /// <typeparam name="T">The type of the value of the behavior.</typeparam>
        /// <param name="value">The lazily computed value of the behavior.</param>
        /// <returns>A behavior with a lazily computed constant value.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Behavior<T> ConstantLazy<T>(Lazy<T> value) => BehaviorInternal.ConstantLazyImpl(value);

        /// <summary>
        ///     Creates a writable behavior that uses the last value if <see cref="BehaviorSinkExtensionMethods.Send{T}" /> is called more than once per
        ///     transaction.
        /// </summary>
        /// <param name="initialValue">The initial value of the behavior.</param>
        /// <typeparam name="T">The type of values in the behavior sink.</typeparam>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static BehaviorSink<T> CreateSink<T>(T initialValue) => BehaviorInternal.CreateSinkImpl(initialValue);

        /// <summary>
        ///     Creates a writable behavior that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="BehaviorSinkExtensionMethods.Send{T}" /> is called more than once per transaction.
        /// </summary>
        /// <param name="initialValue">The initial value of the behavior.</param>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="BehaviorSinkExtensionMethods.Send{T}" /> is called more than once per
        ///     transaction.
        /// </param>
        /// <typeparam name="T">The type of values in the behavior sink.</typeparam>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static BehaviorSink<T> CreateSink<T>(T initialValue, Func<T, T, T> coalesce) => BehaviorInternal.CreateSinkImpl(initialValue, coalesce);

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
        [MethodImpl(MethodImplOptions.NoInlining)]
        public (Behavior<T> Behavior, TCaptures Captures) WithCaptures<TCaptures>(
            Func<LoopedBehavior<T>, (Behavior<T> Behavior, TCaptures Captures)> f) =>
            TransactionInternal.Apply(
                (trans, _) =>
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
            this.WithCaptures(l => (Behavior: f(l), Captures: UnitInternal.Value)).Behavior;
    }
}