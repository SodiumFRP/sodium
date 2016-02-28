using System;
using System.Windows.Threading;

namespace PetrolPump
{
    public static class DispatcherExtensionMethods
    {
        public static void InvokeIfNecessary(this Dispatcher dispatcher, Action action)
        {
            if (dispatcher.CheckAccess())
            {
                action();
            }
            else
            {
                dispatcher.Invoke(action);
            }
        }
    }
}