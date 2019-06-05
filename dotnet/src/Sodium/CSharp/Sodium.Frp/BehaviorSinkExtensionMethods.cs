using System;
using System.Runtime.CompilerServices;

namespace Sodium.Frp
{
    public static class BehaviorSinkExtensionMethods
    {
        /// <summary>
        ///     Send a value, modifying the value of the behavior.  This method may not be called from inside handlers registered with
        ///     <see cref="StreamExtensionMethods.Listen{T}(Stream{T}, Action{T})" /> or <see cref="CellExtensionMethods.Listen{T}(Cell{T}, Action{T})" />.
        ///     An exception will be thrown, because sinks are for interfacing I/O to FRP only.  They are not meant to be used to
        ///     define new primitives.
        /// </summary>
        /// <typeparam name="T">The type of the behavior sink.</typeparam>
        /// <param name="b">The behavior sink.</param>
        /// <param name="a">The value to send.</param>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static void Send<T>(this BehaviorSink<T> b, T a) => b.SendImpl(a);

        /// <summary>
        ///     Return a reference to this <see cref="BehaviorSink{T}" /> as a <see cref="Behavior{T}" />.
        /// </summary>
        /// <typeparam name="T">The type of the behavior sink.</typeparam>
        /// <param name="b">The behavior sink.</param>
        /// <returns>A reference to this <see cref="BehaviorSink{T}" /> as a <see cref="Behavior{T}" />.</returns>
        public static Behavior<T> AsBehavior<T>(this BehaviorSink<T> b) => b;
    }
}