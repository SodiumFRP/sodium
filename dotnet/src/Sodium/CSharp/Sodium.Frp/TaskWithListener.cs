using System.Runtime.CompilerServices;
using System.Threading.Tasks;

namespace Sodium.Frp
{
    public class TaskWithListener
    {
        private readonly Task task;

        // ReSharper disable once NotAccessedField.Local - Used to keep object from being garbage collected
        private readonly IListener listener;

        public TaskWithListener(Task task, IListener listener)
        {
            this.task = task;
            this.listener = listener;
        }

        public TaskAwaiter GetAwaiter() => this.task.GetAwaiter();
    }

    public class TaskWithListener<T> : TaskWithListener
    {
        private readonly Task<T> task;

        public TaskWithListener(Task<T> task, IListener listener)
            : base(task, listener) => this.task = task;

        public new TaskAwaiter<T> GetAwaiter() => this.task.GetAwaiter();
    }
}