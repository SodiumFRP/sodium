using System;
using System.Collections.Generic;
using System.Threading;
using NUnit.Framework;
using Sodium.Frp.Time;
using Sodium.Functional;

namespace Sodium.Frp.Tests
{
    [TestFixture]
    public class TimerTests
    {
        [Test]
        public void SimultaneousTimerEvents()
        {
            TimerSystem<DateTime> ts = new SystemClockTimerSystem(e => { });
            Behavior<DateTime> time = ts.Time;
            List<DateTime> l = new List<DateTime>();
            Transaction.RunVoid(
                () =>
                {
                    DateTime now = time.Sample();
                    Stream<DateTime> a1 = ts.At(Cell.Constant(Maybe.Some(now.AddMilliseconds(99))));
                    Stream<DateTime> a2 = ts.At(Cell.Constant(Maybe.Some(now.AddMilliseconds(100))));
                    Stream<DateTime> a3 = ts.At(Cell.Constant(Maybe.Some(now.AddMilliseconds(100))));
                    Stream<DateTime> m = a1.OrElse(a2).OrElse(a3);
                    m.Listen(l.Add);
                });
            Thread.Sleep(200);
            Assert.That(l.Count, Is.EqualTo(2));
        }
    }
}