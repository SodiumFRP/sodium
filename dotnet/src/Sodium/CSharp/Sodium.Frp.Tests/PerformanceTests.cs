using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests
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
                CellStreamSink<int> s1 = Cell.CreateStreamSink<int>();
                CellStreamSink<int> s2 = Cell.CreateStreamSink<int>();
                TestObject[] l = Enumerable.Range(0, 5000).Select(_ => new TestObject(loop, s1, s2)).ToArray();
                loop.Loop(s.Snapshot(l.Select(o => o.Cell).Lift()).Map(o => o.All(v => v == 0)));
                return l;
            });
            int[] values = obj.Select(v => v.CurrentValue).ToArray();
            CollectionAssert.AreEqual(Enumerable.Range(1, 5000).Select(_ => 0), values);
        }

        private class TestObject
        {
            // ReSharper disable once NotAccessedField.Local
            private readonly IListener l;
            private Lazy<int> currentValue = new Lazy<int>(() => default(int));

            public TestObject(Stream<bool> s, Stream<int> s1, Stream<int> s2)
            {
                (Cell<int> cell, IStrongListener l) = Transaction.Run(() =>
                {
                    var cellLocal = s.Map(v => v ? 1 : 0).OrElse(s1).OrElse(s2).Hold(0);
                    var cell2 = s1.Snapshot(cellLocal, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right)).Map(v => v + 1)).Hold(3);
                    var cell3 = s1.Snapshot(cellLocal, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right)).Map(v => v + 1)).Hold(3);
                    var cell4 = s1.Snapshot(cellLocal, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right)).Map(v => v + 1)).Hold(3);
                    var cell5 = s1.Snapshot(cellLocal, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right)).Map(v => v + 1)).Hold(3);
                    var cell6 = s1.Snapshot(cellLocal, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right)).Map(v => v + 1)).Hold(3);
                    var cell7 = s1.Snapshot(cellLocal, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right)).Map(v => v + 1)).Hold(3);
                    var cell8 = s1.Snapshot(cellLocal, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right)).Map(v => v + 1)).Hold(3);
                    var cell9 = s1.Snapshot(cellLocal, (left, right) => left + right).Filter(v => v > 5).OrElse(s.Snapshot(s1.Hold(0).Lift(s2.Hold(1), (left, right) => left + right)).Map(v => v + 1)).Hold(3);

                    this.currentValue = cellLocal.SampleLazy();
                    var lLocal = cellLocal.Updates().Listen(v => this.CurrentValue = v);

                    return (cellLocal, lLocal);
                });

                this.Cell = cell;
                this.l = l;
            }

            public Cell<int> Cell { get; }

            public int CurrentValue
            {
                get => this.currentValue.Value;
                private set => this.currentValue = new Lazy<int>(() => value);
            }
        }

        [Test]
        public void TestRunConstruct()
        {
            var objects = Transaction.Run(() =>
            {
                IReadOnlyList<TestObject2> o2 = Enumerable.Range(0, 10000).Select(n => new TestObject2(n, n < 1500, Stream.Never<bool>())).ToArray();
                CellSink<IReadOnlyList<TestObject2>> objectsLocal = Cell.CreateSink(o2);

                return objectsLocal;
            });

            Transaction.Run(
                () =>
                {
                    objects.Send(
                        Enumerable.Range(0, 20000)
                            .Select(n => new TestObject2(n, n < 500, Stream.Never<bool>()))
                            .ToArray());
                    return Unit.Value;
                });
        }

        [Test]
        public void TestRunConstruct2()
        {
            var (objectsAndIsSelected, selectAllStream, objects) = Transaction.Run(() =>
            {
                CellLoop<bool?> allSelectedCellLoop = Cell.CreateLoop<bool?>();
                StreamSink<Unit> toggleAllSelectedStreamLocal = Stream.CreateSink<Unit>();
                Stream<bool> selectAllStreamLocal = toggleAllSelectedStreamLocal.Snapshot(allSelectedCellLoop).Map(a => a != true);

                IReadOnlyList<TestObject2> o2 = Enumerable.Range(0, 10000).Select(n => new TestObject2(n, n < 1500, selectAllStreamLocal)).ToArray();
                CellSink<IReadOnlyList<TestObject2>> objectsLocal = Cell.CreateSink(o2);

                var objectsAndIsSelectedLocal = objectsLocal.Map(oo => oo.Select(o => o.IsSelected.Map(s => new { Object = o, IsSelected = s })).Lift()).SwitchC();

                bool defaultValue = o2.Count < 1;
                Cell<bool?> allSelected =
                    objectsAndIsSelectedLocal.Map(
                        oo =>
                            !oo.Any()
                                ? defaultValue
                                : (oo.All(o => o.IsSelected)
                                    ? true
                                    : (oo.All(o => !o.IsSelected) ? (bool?)false : null)));
                allSelectedCellLoop.Loop(allSelected);

                return (objectsAndIsSelectedLocal, selectAllStreamLocal, objectsLocal);
            });

            List<int> @out = new List<int>();
            using (Transaction.Run(
                () => objectsAndIsSelected.Map(oo => oo.Count(o => o.IsSelected))
                    .Values().Listen(@out.Add)))
            {
                Transaction.Run(() =>
                {
                    objects.Send(
                        Enumerable.Range(0, 20000)
                            .Select(n => new TestObject2(n, n < 500, selectAllStream))
                            .ToArray());
                    return Unit.Value;
                });
            }

            CollectionAssert.AreEqual(new[] { 1500, 500 }, @out);
        }

        private class TestObject2
        {
            public TestObject2(int id, bool initialIsSelected, Stream<bool> selectAllStream)
            {
                this.Id = id;
                this.IsSelectedStreamSink = Stream.CreateSink<bool>();
                this.IsSelected = selectAllStream.OrElse(this.IsSelectedStreamSink).Hold(initialIsSelected);
            }

            public int Id { get; }
            public StreamSink<bool> IsSelectedStreamSink { get; }
            public Cell<bool> IsSelected { get; }
        }
    }
}
