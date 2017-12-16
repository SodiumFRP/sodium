using System.Collections.Generic;
using System.Threading.Tasks;

namespace Sodium.Utils
{
    internal class AsyncAutoResetEvent
    {
        private static readonly Task Completed = Task.FromResult(true);

        private readonly Queue<TaskCompletionSource<bool>> waits = new Queue<TaskCompletionSource<bool>>();
        private bool signaled;

        public void Set()
        {
            TaskCompletionSource<bool> toRelease = null;
            lock (waits)
            {
                if (waits.Count > 0)
                    toRelease = waits.Dequeue();
                else if (!signaled)
                    signaled = true;
            }

            toRelease?.SetResult(true);
        }

        public Task WaitAsync()
        {
            lock (waits)
            {
                if (signaled)
                {
                    signaled = false;
                    return Completed;
                }
                else
                {
                    var tcs = new TaskCompletionSource<bool>();
                    waits.Enqueue(tcs);
                    return tcs.Task;
                }
            }
        }
    }
}
