using System;

namespace Sodium
{
    /// <summary>
    ///     An object which allows for arbitrary cleanup code to safely run when this object is garbage collected.
    /// </summary>
    public class Cleanup
    {
        // ReSharper disable once PrivateFieldCanBeConvertedToLocalVariable
        private readonly Stream<Unit> stream;

        private readonly Action cleanup;

        public Cleanup(Action cleanup)
        {
            this.cleanup = cleanup;

            this.stream = Stream.Never<Unit>();
            this.stream.AttachListener(Listener.Create(cleanup));
        }

        /// <summary>
        ///     Force the cleanup to happen now rather than waiting for this object to be garbage collected.
        /// </summary>
        public void CleanupNow() => this.cleanup();
    }
}