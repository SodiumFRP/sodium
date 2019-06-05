using System.Runtime.CompilerServices;

namespace Sodium.Frp
{
    public static class CleanupExtensionMethods
    {
        /// <summary>
        ///     Force the cleanup to happen now rather than waiting for this object to be garbage collected.
        /// </summary>
        /// <param name="c">The cleanup object.</param>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static void CleanupNow(this Cleanup c) => c.CleanupNowImpl();
    }
}