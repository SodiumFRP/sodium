using System;
using System.Linq;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class PerformanceTests
    {
        [Test]
        public void TestMerge()
        {
            StreamSink<Unit> s = Stream.CreateSink<Unit>();
            var obj = Transaction.Run(() =>
            {
                StreamLoop<bool> loop = Stream.CreateLoop<bool>();
                DiscreteCellStreamSink<int> s1 = new DiscreteCellStreamSink<int>();
                DiscreteCellStreamSink<int> s2 = new DiscreteCellStreamSink<int>();
                TestObject[] l = Enumerable.Range(0, 5000).Select(_ => new TestObject(loop, s1, s2)).ToArray();
                loop.Loop(s.Snapshot(l.Select(o => o.Cell).Lift().Cell).Map(o => o.All(v => v == 0)));
                return l;
            });
            int[] values = obj.Select(v => v.CurrentValue).ToArray();
        }

        private class TestObject
        {
            // ReSharper disable once NotAccessedField.Local
            private readonly IListener l;
            private Lazy<int> currentValue = new Lazy<int>(() => default(int));

            public TestObject(Stream<bool> s, Stream<int> s1, Stream<int> s2)
            {
                (DiscreteCell<int> cell, IStrongListener l) = Transaction.Run(() =>
                {
                    var cellLocal = s.Map(v => v ? 1 : 0).OrElse(s1).OrElse(s2).Hold(0);
                    var cell2 = s1.Snapshot(cellLocal.Cell, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right).Cell).Map(v => v + 1)).Hold(3);
                    var cell3 = s1.Snapshot(cellLocal.Cell, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right).Cell).Map(v => v + 1)).Hold(3);
                    var cell4 = s1.Snapshot(cellLocal.Cell, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right).Cell).Map(v => v + 1)).Hold(3);
                    var cell5 = s1.Snapshot(cellLocal.Cell, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right).Cell).Map(v => v + 1)).Hold(3);
                    var cell6 = s1.Snapshot(cellLocal.Cell, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right).Cell).Map(v => v + 1)).Hold(3);
                    var cell7 = s1.Snapshot(cellLocal.Cell, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right).Cell).Map(v => v + 1)).Hold(3);
                    var cell8 = s1.Snapshot(cellLocal.Cell, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right).Cell).Map(v => v + 1)).Hold(3);
                    var cell9 = s1.Snapshot(cellLocal.Cell, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right).Cell).Map(v => v + 1)).Hold(3);

                    var lLocal = Transaction.Run(() =>
                    {
                        this.currentValue = cellLocal.Cell.SampleLazy();
                        return cellLocal.Updates.Listen(v => this.CurrentValue = v);
                    });

                    return (cellLocal, lLocal);
                });

                this.Cell = cell;
                this.l = l;
            }

            public DiscreteCell<int> Cell { get; }

            public int CurrentValue
            {
                get { return this.currentValue.Value; }
                private set { this.currentValue = new Lazy<int>(() => value); }
            }
        }
    }
}
