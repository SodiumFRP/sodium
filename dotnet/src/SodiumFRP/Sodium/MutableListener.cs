namespace Sodium
{
    public class MutableListener : IListener
    {
        private readonly WeakMutableListener weakMutableListener = new WeakMutableListener();
        private IListener listener;

        public void SetListener(IListener listener)
        {
            this.listener = listener;
            this.weakMutableListener.WeakListener = listener.GetListenerWithWeakReference();
        }

        public void Unlisten()
        {
            this.listener?.Unlisten();
        }

        public IListenerWithWeakReference GetListenerWithWeakReference() => this.weakMutableListener;

        private class WeakMutableListener : IListenerWithWeakReference
        {
            public IListenerWithWeakReference WeakListener;

            public void Unlisten()
            {
                this.WeakListener?.Unlisten();
            }
        }
    }
}