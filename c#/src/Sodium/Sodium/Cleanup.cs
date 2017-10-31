using System;

namespace Sodium
{
    /// <summary>
    /// An object which allows for arbitrary cleanup code to safely run when this object is garbage collected.
    /// </summary>
    public class Cleanup
    {
        // ReSharper disable once PrivateFieldCanBeConvertedToLocalVariable
        private readonly Stream<Unit> stream;

        public Cleanup(Action cleanup)
        {
            this.stream = Stream.Never<Unit>();
            this.stream.AttachListener(Listener.Create(cleanup));
        }
    }
}