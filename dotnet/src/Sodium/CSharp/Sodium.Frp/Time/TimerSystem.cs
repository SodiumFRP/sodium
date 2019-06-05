using System;
using System.Collections.Generic;
using Sodium.Functional;

namespace Sodium.Frp.Time
{
    public class TimerSystem<T> : ITimerSystem<T>
        where T : IComparable<T>
    {
        private readonly ITimerSystemImplementation<T> implementation;

        private readonly Queue<Event> eventQueue = new Queue<Event>();

        public TimerSystem(ITimerSystemImplementation<T> implementation, Action<Exception> handleException)
        {
            this.implementation = implementation;
            this.implementation.Start(handleException);
            BehaviorSink<T> timeSink = new BehaviorSink<T>(this.implementation.Now);
            this.Time = timeSink;
            Transaction.OnStart(
                () =>
                {
                    T t = this.implementation.Now;
                    this.implementation.RunTimersTo(t);
                    while (true)
                    {
                        Event ev = null;
                        // Pop all events earlier than t.
                        lock (this.eventQueue)
                        {
                            if (this.eventQueue.Count > 0)
                            {
                                Event tempEvent = this.eventQueue.Peek();
                                if (tempEvent != null && tempEvent.Time.CompareTo(t) <= 0)
                                {
                                    ev = this.eventQueue.Dequeue();
                                }
                            }
                        }

                        if (ev != null)
                        {
                            timeSink.Send(ev.Time);
                            ev.Alarm.Send(ev.Time);
                        }
                        else
                        {
                            break;
                        }
                    }

                    timeSink.Send(t);
                });
        }

        /// <summary>
        ///     Gets a behavior giving the current clock time.
        /// </summary>
        public Behavior<T> Time { get; }

        private class Event
        {
            internal Event(T time, StreamSink<T> alarm)
            {
                this.Time = time;
                this.Alarm = alarm;
            }

            internal readonly T Time;
            internal readonly StreamSink<T> Alarm;
        }

        /// <summary>
        ///     A timer that fires at the specified time.
        /// </summary>
        /// <param name="t">The time to fire at.</param>
        /// <returns>A stream which fires at the specified time.</returns>
        public Stream<T> At(Cell<Maybe<T>> t)
        {
            StreamSink<T> alarm = new StreamSink<T>();
            Maybe<ITimer> currentTimer = Maybe.None;
            IListener l = t.Listen(
                m =>
                {
                    currentTimer.MatchSome(timer => timer.Cancel());
                    currentTimer = m.Match(
                        time => Maybe.Some(
                            this.implementation.SetTimer(
                                time,
                                () =>
                                {
                                    lock (this.eventQueue)
                                    {
                                        this.eventQueue.Enqueue(new Event(time, alarm));
                                    }
                                    // Open and close a transaction to trigger queued
                                    // events to run.
                                    Transaction.RunVoid(() => { });
                                })),
                        () => Maybe.None);
                });
            return alarm.AttachListener(l);
        }
    }
}