namespace SodiumFRP
{
    public static class MutableListenerExtensionMethods
    {

        public static void SetListener(this MutableListener m, IListener listener) => m.SetListenerImpl(listener);
        public static void ClearListener(this MutableListener m) => m.ClearListenerImpl();

        public static void Unlisten(this MutableListener m)
        {
            IListener l = m;
            l.Unlisten();
        }

        public static IListenerWithWeakReference GetListenerWithWeakReference(this MutableListener m)
        {
            IListener l = m;
            return l.GetListenerWithWeakReference();
        }
    }
}