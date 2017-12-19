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
            lock (this.waits)
            {
                if (this.waits.Count > 0)
                {
                    toRelease = this.waits.Dequeue();
                }
                else if (!this.signaled)
                {
                    this.signaled = true;
                }
            }

            toRelease?.SetResult(true);
        }

        public Task WaitAsync()
        {
            lock (this.waits)
            {
                if (this.signaled)
                {
                    this.signaled = false;
                    return Completed;
                }
                TaskCompletionSource<bool> tcs = new TaskCompletionSource<bool>();
                this.waits.Enqueue(tcs);
                return tcs.Task;
            }
        }
    }
}