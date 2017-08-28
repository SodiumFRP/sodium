namespace Sodium
{
    public class MutableListener : IListener
    {
        private readonly WeakMutableListener weakMutableListener = new WeakMutableListener();
        private IListener listener;

        public void SetListener(IListener listener)
        {
            this.listener = listener;
            this.weakMutableListener.WeakListener = listener.GetWeakListener();
        }

        public void Unlisten()
        {
            this.listener?.Unlisten();
        }

        public IWeakListener GetWeakListener()
        {
            return this.weakMutableListener;
        }

        private class WeakMutableListener : IWeakListener
        {
            public IWeakListener WeakListener;

            public void Unlisten()
            {
                this.WeakListener?.Unlisten();
            }
        }
    }
}