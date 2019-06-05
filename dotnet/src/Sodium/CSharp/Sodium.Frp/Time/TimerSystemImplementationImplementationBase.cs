using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Sodium.Frp.Time
{
    public abstract class TimerSystemImplementationImplementationBase<T> : ITimerSystemImplementation<T>
        where T : IComparable
    {
        private readonly object lockObject = new object();
        private readonly SortedSet<SimpleTimer> timers = new SortedSet<SimpleTimer>();
        private readonly object cancellationTokenSourceLock = new object();
        private CancellationTokenSource cancellationTokenSource;

        private long nextSeq;

        private TimeSpan TimeUntilNext(T now)
        {
            while (true)
            {
                SimpleTimer fired = null;
                TimeSpan waitTime;
                lock (this.lockObject)
                {
                    if (this.timers.Count < 1)
                    {
                        waitTime = TimeSpan.FromSeconds(1000);
                    }
                    else
                    {
                        // How long till the first timer?
                        SimpleTimer timer = this.timers.First();
                        waitTime = this.SubtractTimes(timer.Time, now);
                        if (waitTime <= TimeSpan.Zero)
                        {
                            waitTime = TimeSpan.Zero;
                            fired = timer;
                            this.timers.Remove(fired);
                        }
                    }
                }

                if (fired != null)
                {
                    fired.Callback();
                }
                else
                {
                    return waitTime;
                }
            }
        }

        protected abstract TimeSpan SubtractTimes(T first, T second);

        public void Start(Action<Exception> handleException)
        {
            Task.Run(
                async () =>
                {
                    while (true)
                    {
                        try
                        {
                            TimeSpan waitTime = this.TimeUntilNext(this.Now);
                            if (waitTime > TimeSpan.Zero)
                            {
                                try
                                {
                                    lock (this.cancellationTokenSourceLock)
                                    {
                                        this.cancellationTokenSource = new CancellationTokenSource();
                                    }

                                    await Task.Delay(waitTime, this.cancellationTokenSource.Token);
                                }
                                catch (OperationCanceledException)
                                {
                                }
                                finally
                                {
                                    lock (this.cancellationTokenSourceLock)
                                    {
                                        this.cancellationTokenSource = null;
                                    }
                                }
                            }
                        }
                        catch (Exception e)
                        {
                            handleException(e);
                        }
                    }
                    // ReSharper disable once FunctionNeverReturns - This is a timer loop.  It should run until the application ends.
                });
        }

        public ITimer SetTimer(T time, Action callback)
        {
            SimpleTimer timer = new SimpleTimer(this, time, callback);
            lock (this.lockObject)
            {
                this.timers.Add(timer);
                lock (this.cancellationTokenSourceLock)
                {
                    this.cancellationTokenSource?.Cancel();
                }
            }
            return timer;
        }

        public void RunTimersTo(T now) => this.TimeUntilNext(now);

        public abstract T Now { get; }

        private class SimpleTimer : ITimer, IComparable<SimpleTimer>
        {
            private readonly TimerSystemImplementationImplementationBase<T> implementation;
            private readonly long seq;

            internal readonly T Time;
            internal readonly Action Callback;

            internal SimpleTimer(TimerSystemImplementationImplementationBase<T> implementation, T time, Action callback)
            {
                this.implementation = implementation;
                this.Time = time;
                this.Callback = callback;

                lock (implementation.lockObject)
                {
                    this.seq = implementation.nextSeq++;
                }
            }

            public void Cancel()
            {
                lock (this.implementation.lockObject)
                {
                    this.implementation.timers.Remove(this);
                }
            }

            public int CompareTo(SimpleTimer o)
            {
                int timeComparison = this.Time.CompareTo(o.Time);
                return timeComparison != 0 ? timeComparison : this.seq.CompareTo(o.seq);
            }

            public void Dispose()
            {
                this.Cancel();
            }
        }
    }
}