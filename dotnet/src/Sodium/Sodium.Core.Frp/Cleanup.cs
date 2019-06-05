using System;

namespace Sodium.Frp
{
    /// <summary>
    ///     An object which allows for arbitrary cleanup code to safely run when this object is garbage collected.
    /// </summary>
    public class Cleanup
    {
        // ReSharper disable once PrivateFieldCanBeConvertedToLocalVariable
        // ReSharper disable once NotAccessedField.Local
        private Stream<UnitInternal> stream;

        public Cleanup(Action cleanup)
        {
            Stream<UnitInternal> stream = StreamInternal.NeverImpl<UnitInternal>();
            stream.AttachListenerImpl(ListenerInternal.CreateFromAction(cleanup));

            this.stream = stream;
        }

        internal void CleanupNowImpl() => this.stream = null;
    }
}