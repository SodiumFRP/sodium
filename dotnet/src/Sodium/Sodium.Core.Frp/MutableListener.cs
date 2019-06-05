namespace Sodium.Frp
{
    public class MutableListener : IListener
    {
        private readonly WeakMutableListener weakMutableListener = new WeakMutableListener();
        private IListener listener;

        internal void SetListenerImpl(IListener listener)
        {
            this.listener = listener;
            this.weakMutableListener.WeakListener = listener?.GetListenerWithWeakReference();
        }

        internal void ClearListenerImpl()
        {
            this.listener = null;
            this.weakMutableListener.WeakListener = null;
        }

        void IListener.Unlisten() => this.listener?.Unlisten();
        IListenerWithWeakReference IListener.GetListenerWithWeakReference() => this.weakMutableListener;

        private class WeakMutableListener : IListenerWithWeakReference
        {
            public IListenerWithWeakReference WeakListener;

            void IListenerWithWeakReference.Unlisten() => this.WeakListener?.Unlisten();
        }
    }
}