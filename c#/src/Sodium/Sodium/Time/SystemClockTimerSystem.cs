using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Sodium.Time
{
    /// <summary>
    /// A timer system using the current system clock.
    /// </summary>
    public class SystemClockTimerSystem : TimerSystem<DateTime>
    {
        public SystemClockTimerSystem(Action<Exception> handleException)
            : base(new Implementation(handleException))
        {
        }

        internal class Implementation : ITimerSystemImplementation<DateTime>
        {
            private readonly object lockObject = new object();
            private readonly SortedSet<SimpleTimer> timers = new SortedSet<SimpleTimer>();
            private readonly object cancellationTokenSourceLock = new object();
            private CancellationTokenSource cancellationTokenSource;

            private long nextSeq;

            private TimeSpan TimeUntilNext(DateTime now)
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
                            waitTime = timer.Time - now;
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

            public Implementation(Action<Exception> handleException)
            {
                Task.Run(async () =>
                {
                    while (true)
                    {
                        try
                        {
                            TimeSpan waitTime = this.TimeUntilNext(DateTime.Now);
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

            public ITimer SetTimer(DateTime time, Action callback)
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

            public void RunTimersTo(DateTime now) => this.TimeUntilNext(now);

            public DateTime Now => DateTime.Now;

            private class SimpleTimer : ITimer, IComparable<SimpleTimer>
            {
                private readonly Implementation implementation;
                private readonly long seq;

                internal readonly DateTime Time;
                internal readonly Action Callback;

                internal SimpleTimer(Implementation implementation, DateTime time, Action callback)
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
                    if (this.Time < o.Time)
                    {
                        return -1;
                    }

                    if (this.Time > o.Time)
                    {
                        return 1;
                    }

                    if (this.seq < o.seq)
                    {
                        return -1;
                    }

                    if (this.seq > o.seq)
                    {
                        return 1;
                    }

                    return 0;
                }

                public void Dispose()
                {
                }
            }
        }
    }
}