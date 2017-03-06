using System.Runtime.CompilerServices;
using System.Threading.Tasks;

namespace Sodium
{
    public class TaskWithListener<T>
    {
        private readonly Task<T> task;
        // ReSharper disable once NotAccessedField.Local - Used to keep object from being garbage collected
        private readonly IListener listener;

        public TaskWithListener(Task<T> task, IListener listener)
        {
            this.task = task;
            this.listener = listener;
        }

        public TaskAwaiter<T> GetAwaiter()
        {
            return this.task.GetAwaiter();
        }
    }
}