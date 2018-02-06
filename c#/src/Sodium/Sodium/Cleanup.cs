using System;

namespace Sodium
{
    /// <summary>
    ///     An object which allows for arbitrary cleanup code to safely run when this object is garbage collected.
    /// </summary>
    public class Cleanup
    {
        // ReSharper disable once PrivateFieldCanBeConvertedToLocalVariable
        // ReSharper disable once NotAccessedField.Local
        private Stream<Unit> stream;

        public Cleanup(Action cleanup)
        {
            Stream<Unit> stream = Stream.Never<Unit>();
            stream.AttachListener(Listener.Create(cleanup));

            this.stream = stream;
        }

        /// <summary>
        ///     Force the cleanup to happen now rather than waiting for this object to be garbage collected.
        /// </summary>
        public void CleanupNow()
        {
            this.stream = null;
        }
    }
}