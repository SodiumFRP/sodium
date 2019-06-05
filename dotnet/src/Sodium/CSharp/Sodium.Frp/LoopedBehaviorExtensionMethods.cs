namespace Sodium.Frp
{
    public static class LoopedBehaviorExtensionMethods
    {
        /// <summary>
        ///     Return a reference to this <see cref="LoopedBehavior{T}" /> as a <see cref="Behavior{T}" />.
        /// </summary>
        /// <typeparam name="T">The type of the looped behavior.</typeparam>
        /// <param name="b">The looped behavior.</param>
        /// <returns>A reference to this <see cref="LoopedBehavior{T}" /> as a <see cref="Behavior{T}" />.</returns>
        public static Behavior<T> AsBehavior<T>(this LoopedBehavior<T> b) => b;
    }
}
