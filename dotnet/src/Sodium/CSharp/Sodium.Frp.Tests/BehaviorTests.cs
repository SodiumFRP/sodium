using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests
{
    [TestFixture]
    public class BehaviorTests
    {
        [Test]
        public void TestHold()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            Cell<int> c = s.Hold(0);
            List<int> @out = new List<int>();
            IListener l = c.Listen(@out.Add);
            s.Send(2);
            s.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 0, 2, 9 }, @out);
        }

        [Test]
        public void TestSendNull()
        {
            CellSink<string> c = Cell.CreateSink(string.Empty);
            List<string> @out = new List<string>();
            IListener l = c.Listen(@out.Add);
            c.Send("0");
            c.Send(null);
            c.Send("1");
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { string.Empty, "0", null, "1" }, @out);
        }

        [Test]
        public void TestHoldUpdates()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            Cell<int> c = s.Hold(0);
            List<int> @out = new List<int>();
            IListener l = c.Updates().Listen(@out.Add);
            s.Send(2);
            s.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 9 }, @out);
        }

        [Test]
        public void TestSnapshot()
        {
            BehaviorSink<int> b = Behavior.CreateSink(0);
            StreamSink<long> trigger = Stream.CreateSink<long>();
            List<string> @out = new List<string>();
            IListener l = trigger.Snapshot(b, (x, y) => x + " " + y).Listen(@out.Add);
            trigger.Send(100L);
            b.Send(2);
            trigger.Send(200L);
            b.Send(9);
            b.Send(1);
            trigger.Send(300L);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "100 0", "200 2", "300 1" }, @out);
        }

        [Test]
        public void TestListen()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = c.Listen(@out.Add);
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9, 2, 7 }, @out);
        }

        [Test]
        public void TestListenOnce()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Values().ListenOnce(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9 }, @out);
        }

        [Test]
        public void TestListenOnceUpdates()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Updates().ListenOnce(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2 }, @out);
        }

        [Test]
        public async Task TestListenOnceAsync()
        {
            CellSink<int> c = Cell.CreateSink(9);
            int result = await Transaction.Run(() => c.Values().ListenOnceAsync());
            c.Send(2);
            c.Send(7);
            Assert.AreEqual(9, result);
        }

        [Test]
        public void TestUpdates()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = c.Updates().Listen(@out.Add);
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestValues()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Values().Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9, 2, 7 }, @out);
        }

        private class TestObject
        {
            private readonly StreamSink<Unit> removeStreamSink;
            private readonly StreamSink<int> changeNumber1StreamSink;
            private readonly StreamSink<int> changeNumber2StreamSink;

            public TestObject(int n1, int n2)
            {
                this.removeStreamSink = Stream.CreateSink<Unit>();
                this.RemoveStream = this.removeStreamSink;

                this.changeNumber1StreamSink = Stream.CreateSink<int>();
                this.Number1Cell = this.changeNumber1StreamSink.Hold(n1);
                this.changeNumber2StreamSink = Stream.CreateSink<int>();
                this.Number2Cell = this.changeNumber2StreamSink.Hold(n2);
                this.BothNumbersCell = this.Number1Cell.Lift(this.Number2Cell, (number1, number2) => (Number1: number1, Number2: number2)).Values().Hold((Number1: 99, Number2: 99));
            }

            public Stream<Unit> RemoveStream { get; }
            public Cell<int> Number1Cell { get; }
            public Cell<int> Number2Cell { get; }
            public Cell<(int Number1, int Number2)> BothNumbersCell { get; }

            public void Remove()
            {
                this.removeStreamSink.Send(Unit.Value);
            }

            public void ChangeNumber1(int n)
            {
                this.changeNumber1StreamSink.Send(n);
            }

            public void ChangeNumber2(int n)
            {
                this.changeNumber2StreamSink.Send(n);
            }
        }

        [Test]
        public void TestCellLoopComplex()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            StreamSink<(int Number1, int Number2)> addItemStreamSink = Stream.CreateSink<(int Number1, int Number2)>();
            StreamSink<IReadOnlyList<TestObject>> removeItemsStreamSink = Stream.CreateSink<IReadOnlyList<TestObject>>();
            Cell<IReadOnlyList<TestObject>> listCell = Transaction.Run(() =>
            {
                CellLoop<IReadOnlyList<TestObject>> listCellLoop = new CellLoop<IReadOnlyList<TestObject>>();
                Cell<IReadOnlyList<TestObject>> listCellLocal =
                    addItemStreamSink.OrElse(s.Map(v => (Number1: v, Number2: v)))
                        .Map(o => (Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>)(c => c.Concat(new[] { new TestObject(o.Number1, o.Number2) }).ToArray()))
                        .Merge(removeItemsStreamSink.Map(o => (Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>)(c => c.Except(o).ToArray())), (f, g) => c => g(f(c)))
                        .Snapshot(listCellLoop, (f, c) => f(c))
                        .Hold(new TestObject[0]);
                listCellLoop.Loop(listCellLocal);
                return listCellLocal;
            });
            IListener l2 = Transaction.Run(() =>
            {
                return listCell.Values().Listen(c => Transaction.Post(() =>
                {
                    if (c.Any())
                    {
                        (int n1, int n2) = c.Last().BothNumbersCell.Sample();
                        if (n1 == 9 && n2 == 9)
                        {
                            addItemStreamSink.Send((Number1: 0, Number2: 0));
                        }
                    }
                }));
            });
            IListener l3 = Transaction.Run(() =>
                listCell.Map(c => c.Select(o => o.RemoveStream.MapTo(new[] { o })).Merge((x, y) => x.Concat(y).ToArray())).SwitchS().Listen(o => Transaction.Post(() => removeItemsStreamSink.Send(o))));
            IListener l4 = Transaction.Run(() =>
                listCell.Map(c => c.Any() ? c.Last().Number1Cell.Lift(c.Last().Number2Cell, (x, y) => x == 9 && y == 9).Updates() : Stream.Never<bool>()).SwitchS().Filter(v => v).Listen(_ => Transaction.Post(() => addItemStreamSink.Send((Number1: 0, Number2: 0)))));
            List<IReadOnlyList<(int Number1, int Number2)>> @out = new List<IReadOnlyList<(int Number1, int Number2)>>();
            IListener l = listCell.Map(c => c.Select(o => o.Number1Cell.Lift(o.Number2Cell, (number1, number2) => (Number1: number1, Number2: number2))).Lift()).SwitchC().Listen(@out.Add);
            addItemStreamSink.Send((Number1: 5, Number2: 2));
            addItemStreamSink.Send((Number1: 9, Number2: 2));
            listCell.Sample()[0].Remove();
            addItemStreamSink.Send((Number1: 2, Number2: 9));
            listCell.Sample()[1].ChangeNumber1(9);
            addItemStreamSink.Send((Number1: 9, Number2: 9));
            s.Send(5);
            s.Send(9);
            Transaction.RunVoid(() =>
            {
                addItemStreamSink.Send((Number1: 5, Number2: 5));
                s.Send(5);
            });
            listCell.Sample()[8].ChangeNumber2(9);
            listCell.Sample()[8].ChangeNumber1(9);
            l.Unlisten();
            l2.Unlisten();
            l3.Unlisten();
            l4.Unlisten();

            (int Number1, int Number2)[][] expected =
            {
                new (int Number1, int Number2)[0],
                new[] { (Number1: 5, Number2: 2) },
                new[] { (Number1: 5, Number2: 2), (Number1: 9, Number2: 2) },
                new[] { (Number1: 9, Number2: 2) },
                new[] { (Number1: 9, Number2: 2), (Number1: 2, Number2: 9) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 5, Number2: 5) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 5, Number2: 5), (Number1: 9, Number2: 9) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 5, Number2: 5), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 5, Number2: 5), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 5, Number2: 5) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 5, Number2: 5), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 5, Number2: 9) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 5, Number2: 5), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9) },
                new[] { (Number1: 9, Number2: 2), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 5, Number2: 5), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0), (Number1: 9, Number2: 9), (Number1: 0, Number2: 0) }
            };
            Assert.AreEqual(expected.Length, @out.Count);
            for (int i = 0; i < 16; i++)
            {
                Assert.AreEqual(expected[i].Length, @out[i].Count);
                for (int j = 0; j < expected[i].Length; j++)
                {
                    Assert.AreEqual(expected[i][j], @out[i][j]);
                }
            }
        }

        [Test]
        public void TestCellLoop()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            Cell<int> cell = Transaction.Run(() =>
            {
                CellLoop<int> cellLoop = new CellLoop<int>();
                Cell<int> cellLocal = s.Snapshot(cellLoop, (x, y) => x + y).Hold(1);
                cellLoop.Loop(cellLocal);
                return cellLocal;
            });
            List<int> @out = new List<int>();
            IListener l = cell.Listen(@out.Add);
            s.Send(3);
            s.Send(4);
            s.Send(7);
            s.Send(8);
            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 1, 4, 8, 15, 23 }, @out);
        }

        [Test]
        public void TestCellLoopThrowsException()
        {
            //TODO: adjust the types so that loops can only be created safely through the type system

            Exception actual = null;

            try
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                Cell<int> cell = Transaction.Run(() =>
                {
                    CellLoop<int> cellLoop = new CellLoop<int>();
                    Cell<int> cellLocal = cellLoop.Updates().Filter(v => v % 2 == 0).Map(v => v + 1).Merge(s, (_, r) => r).Hold(1);
                    cellLoop.Loop(cellLocal);
                    return cellLocal;
                });
                List<int> @out = new List<int>();
                IListener l = cell.Listen(@out.Add);
                s.Send(3);
                s.Send(4);
                s.Send(7);
                s.Send(8);
                l.Unlisten();
            }
            catch (AggregateException e)
            {
                actual = e.InnerExceptions.FirstOrDefault(ex => ex.Message == "A dependency cycle was detected.");
            }
            catch (Exception e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("A dependency cycle was detected.", actual.Message);
        }

        [Test]
        public void TestCellLoopSwitchS()
        {
            StreamSink<TestObject> addStreamSink = Stream.CreateSink<TestObject>();
            Cell<IReadOnlyList<TestObject>> cell = Transaction.Run(() =>
            {
                CellLoop<IReadOnlyList<TestObject>> cellLoop = new CellLoop<IReadOnlyList<TestObject>>();
                Cell<IReadOnlyList<TestObject>> cellLocal =
                    cellLoop.Map(oo => oo.Select(o => o.RemoveStream.MapTo(new[] { o })).Merge((x, y) => x.Concat(y).ToArray())).SwitchS().Map(o => (Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>)(c => c.Except(o).ToArray()))
                        .Merge(addStreamSink.Map(o => (Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>)(c => c.Concat(new[] { o }).ToArray())), (f, g) => c => g(f(c)))
                        .Snapshot(cellLoop, (f, c) => f(c))
                        .Hold(new TestObject[0]);
                cellLoop.Loop(cellLocal);
                return cellLocal;
            });
            List<int> @out = new List<int>();
            IListener l = cell.Listen(c => @out.Add(c.Count));
            TestObject t1 = new TestObject(1, 1);
            addStreamSink.Send(t1);
            TestObject t2 = new TestObject(2, 2);
            addStreamSink.Send(t2);
            TestObject t3 = new TestObject(3, 3);
            addStreamSink.Send(t3);
            t2.Remove();
            TestObject t4 = new TestObject(4, 4);
            Transaction.RunVoid(() =>
            {
                addStreamSink.Send(t4);
                t3.Remove();
            });
            TestObject t5 = new TestObject(5, 5);
            addStreamSink.Send(t5);
            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 2, 2, 3 }, @out);
        }

        [Test]
        public void TestCellValues()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Values().Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9, 2, 7 }, @out);
        }

        [Test]
        public void TestCellValuesNoTransaction()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = c.Values().Listen(@out.Add);
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestValueThenMap()
        {
            BehaviorSink<int> b = Behavior.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => Operational.Value(b).Map(x => x + 100).Listen(@out.Add));
            b.Send(2);
            b.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 109, 102, 107 }, @out);
        }

        [Test]
        public void TestCellValuesThenMap()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Values().Map(x => x + 100).Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 109, 102, 107 }, @out);
        }

        [Test]
        public void TestValueThenMerge()
        {
            BehaviorSink<int> b1 = Behavior.CreateSink(9);
            BehaviorSink<int> b2 = Behavior.CreateSink(2);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => Operational.Value(b1).Merge(Operational.Value(b2), (x, y) => x + y).Listen(@out.Add));
            b1.Send(1);
            b2.Send(4);
            Transaction.RunVoid(() =>
            {
                b1.Send(7);
                b2.Send(5);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 11, 1, 4, 12 }, @out);
        }

        [Test]
        public void TestCellValuesThenMerge()
        {
            CellSink<int> c1 = Cell.CreateSink(9);
            CellSink<int> c2 = Cell.CreateSink(2);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c1.Values().Merge(c2.Values(), (x, y) => x + y).Listen(@out.Add));
            c1.Send(1);
            c2.Send(4);
            Transaction.RunVoid(() =>
            {
                c1.Send(7);
                c2.Send(5);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 11, 1, 4, 12 }, @out);
        }

        [Test]
        public void TestValueThenFilter()
        {
            BehaviorSink<int> b = Behavior.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => Operational.Value(b).Filter(x => x % 2 != 0).Listen(@out.Add));
            b.Send(2);
            b.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9, 7 }, @out);
        }

        [Test]
        public void TestCellValuesThenFilter()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Values().Filter(x => x % 2 != 0).Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9, 7 }, @out);
        }

        [Test]
        public void TestValueThenOnce()
        {
            BehaviorSink<int> b = Behavior.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => Operational.Value(b).Once().Listen(@out.Add));
            b.Send(2);
            b.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9 }, @out);
        }

        [Test]
        public void TestCellValuesThenOnce()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Values().Once().Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9 }, @out);
        }

        [Test]
        public void TestValueThenLateListen()
        {
            BehaviorSink<int> b = Behavior.CreateSink(9);
            List<int> @out = new List<int>();
            Stream<int> value = Operational.Value(b);
            b.Send(8);
            IListener l = value.Listen(@out.Add);
            b.Send(2);
            b.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestCellValuesThenLateListen()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            Stream<int> value = c.Values();
            c.Send(8);
            IListener l = value.Listen(@out.Add);
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestMap()
        {
            CellSink<int> c = Cell.CreateSink(6);
            List<string> @out = new List<string>();
            IListener l = c.Map(x => x.ToString()).Listen(@out.Add);
            c.Send(8);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "6", "8" }, @out);
        }

        [Test]
        public void TestMapLateListen()
        {
            CellSink<int> c = Cell.CreateSink(6);
            List<string> @out = new List<string>();
            Cell<string> cm = c.Map(x => x.ToString());
            c.Send(2);
            IListener l = cm.Listen(@out.Add);
            c.Send(8);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "2", "8" }, @out);
        }

        [Test]
        public void TestCalm()
        {
            CellSink<int> c = Cell.CreateSink(2);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Calm().Listen(@out.Add));
            c.Send(2);
            c.Send(2);
            c.Send(4);
            c.Send(2);
            c.Send(4);
            c.Send(4);
            c.Send(2);
            c.Send(2);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 4, 2, 4, 2 }, @out);
        }

        [Test]
        public void TestCalm2()
        {
            CellSink<int> c = Cell.CreateSink(2);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Calm().Listen(@out.Add));
            c.Send(4);
            c.Send(2);
            c.Send(4);
            c.Send(4);
            c.Send(2);
            c.Send(2);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 4, 2, 4, 2 }, @out);
        }

        [Test]
        public void TestApply()
        {
            CellSink<Func<long, string>> cf = Cell.CreateSink<Func<long, string>>(x => "1 " + x);
            CellSink<long> ca = Cell.CreateSink(5L);
            List<string> @out = new List<string>();
            IListener l = ca.Apply(cf).Listen(@out.Add);
            cf.Send(x => "12 " + x);
            ca.Send(6L);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "1 5", "12 5", "12 6" }, @out);
        }

        [Test]
        public void TestLift()
        {
            CellSink<int> c1 = Cell.CreateSink(1);
            CellSink<long> c2 = Cell.CreateSink(5L);
            List<string> @out = new List<string>();
            IListener l = c1.Lift(c2, (x, y) => x + " " + y).Listen(@out.Add);
            c1.Send(12);
            c2.Send(6L);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "1 5", "12 5", "12 6" }, @out);
        }

        [Test]
        public void TestLiftGlitch()
        {
            CellSink<int> c1 = Cell.CreateSink(1);
            Cell<int> c3 = c1.Map(x => x * 3);
            Cell<int> c5 = c1.Map(x => x * 5);
            Cell<string> c = c3.Lift(c5, (x, y) => x + " " + y);
            List<string> @out = new List<string>();
            IListener l = c.Listen(@out.Add);
            c1.Send(2);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "3 5", "6 10" }, @out);
        }

        [Test]
        public void TestLiftFromSimultaneous()
        {
            (CellSink<int> c1, CellSink<int> c2) = Transaction.Run(() =>
            {
                CellSink<int> localC1 = Cell.CreateSink(3);
                CellSink<int> localC2 = Cell.CreateSink(5);
                localC2.Send(7);
                return (localC1, localC2);
            });
            List<int> @out = new List<int>();
            IListener l = c1.Lift(c2, (x, y) => x + y).Listen(@out.Add);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 10 }, @out);
        }

        [Test]
        public void TestHoldIsDelayed()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            Cell<int> h = s.Hold(0);
            Stream<string> pair = s.Snapshot(h, (a, b) => a + " " + b);
            List<string> @out = new List<string>();
            IListener l = pair.Listen(@out.Add);
            s.Send(2);
            s.Send(3);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "2 0", "3 2" }, @out);
        }

        private class Sc
        {
            public readonly Maybe<char> A;
            public readonly Maybe<char> B;
            public readonly Maybe<Cell<char>> Sw;

            public Sc(Maybe<char> a, Maybe<char> b, Maybe<Cell<char>> sw)
            {
                this.A = a;
                this.B = b;
                this.Sw = sw;
            }
        }

        [Test]
        public void TestSwitchC()
        {
            StreamSink<Sc> ssc = Stream.CreateSink<Sc>();
            // Split each field out of SB so we can update multiple behaviors in a
            // single transaction.
            Cell<char> ca = ssc.Map(s => s.A).FilterMaybe().Hold('A');
            Cell<char> cb = ssc.Map(s => s.B).FilterMaybe().Hold('a');
            Cell<Cell<char>> csw = ssc.Map(s => s.Sw).FilterMaybe().Hold(ca);
            Cell<char> co = csw.SwitchC();
            List<char> @out = new List<char>();
            IListener l = co.Listen(@out.Add);
            ssc.Send(new Sc(Maybe.Some('B'), Maybe.Some('b'), Maybe.None));
            ssc.Send(new Sc(Maybe.Some('C'), Maybe.Some('c'), Maybe.Some(cb)));
            ssc.Send(new Sc(Maybe.Some('D'), Maybe.Some('d'), Maybe.None));
            ssc.Send(new Sc(Maybe.Some('E'), Maybe.Some('e'), Maybe.Some(ca)));
            ssc.Send(new Sc(Maybe.Some('F'), Maybe.Some('f'), Maybe.None));
            ssc.Send(new Sc(Maybe.None, Maybe.None, Maybe.Some(cb)));
            ssc.Send(new Sc(Maybe.None, Maybe.None, Maybe.Some(ca)));
            ssc.Send(new Sc(Maybe.Some('G'), Maybe.Some('g'), Maybe.Some(cb)));
            ssc.Send(new Sc(Maybe.Some('H'), Maybe.Some('h'), Maybe.Some(ca)));
            ssc.Send(new Sc(Maybe.Some('I'), Maybe.Some('i'), Maybe.Some(ca)));
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 'A', 'B', 'c', 'd', 'E', 'F', 'f', 'F', 'g', 'H', 'I' }, @out);
        }

        private class Sc2
        {
            public readonly CellSink<int> C;

            public Sc2(int initialValue)
            {
                this.C = Cell.CreateSink(initialValue);
            }
        }

        [Test]
        public void TestSwitchCSimultaneous()
        {
            Sc2 sc1 = new Sc2(0);
            CellSink<Sc2> csc = Cell.CreateSink(sc1);
            Cell<int> co = csc.Map(b => b.C.AsCell()).SwitchC();
            List<int> @out = new List<int>();
            IListener l = co.Listen(@out.Add);
            Sc2 sc2 = new Sc2(3);
            Sc2 sc3 = new Sc2(4);
            Sc2 sc4 = new Sc2(7);
            sc1.C.Send(1);
            sc1.C.Send(2);
            csc.Send(sc2);
            sc1.C.Send(3);
            sc2.C.Send(4);
            sc3.C.Send(5);
            csc.Send(sc3);
            sc3.C.Send(6);
            sc3.C.Send(7);
            Transaction.RunVoid(() =>
            {
                sc3.C.Send(2);
                csc.Send(sc4);
                sc4.C.Send(8);
            });
            sc4.C.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, @out);
        }

        private class Ss
        {
            public readonly char A;
            public readonly char B;
            public readonly Maybe<Stream<char>> Sw;

            public Ss(char a, char b, Maybe<Stream<char>> sw)
            {
                this.A = a;
                this.B = b;
                this.Sw = sw;
            }
        }

        [Test]
        public void TestSwitchS()
        {
            StreamSink<Ss> sss = Stream.CreateSink<Ss>();
            // Split each field out of SB so we can update multiple behaviors in a
            // single transaction.
            Stream<char> sa = sss.Map(s => s.A);
            Stream<char> sb = sss.Map(s => s.B);
            Cell<Stream<char>> csw = sss.Map(s => s.Sw).FilterMaybe().Hold(sa);
            Stream<char> so = csw.SwitchS();
            List<char> @out = new List<char>();
            IListener l = so.Listen(@out.Add);
            sss.Send(new Ss('A', 'a', Maybe.None));
            sss.Send(new Ss('B', 'b', Maybe.None));
            sss.Send(new Ss('C', 'c', Maybe.Some(sb)));
            sss.Send(new Ss('D', 'd', Maybe.None));
            sss.Send(new Ss('E', 'e', Maybe.Some(sa)));
            sss.Send(new Ss('F', 'f', Maybe.None));
            sss.Send(new Ss('G', 'g', Maybe.Some(sb)));
            sss.Send(new Ss('H', 'h', Maybe.Some(sa)));
            sss.Send(new Ss('I', 'i', Maybe.Some(sa)));
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 'A', 'B', 'C', 'd', 'e', 'F', 'G', 'h', 'I' }, @out);
        }

        private class Ss2
        {
            public readonly StreamSink<int> S = Stream.CreateSink<int>();
        }

        [Test]
        public void TestSwitchSSimultaneous()
        {
            Ss2 ss1 = new Ss2();
            BehaviorSink<Ss2> bss = Behavior.CreateSink(ss1);
            Stream<int> so = bss.Map(b => b.S.AsStream()).SwitchS();
            List<int> @out = new List<int>();
            IListener l = so.Listen(@out.Add);
            Ss2 ss2 = new Ss2();
            Ss2 ss3 = new Ss2();
            Ss2 ss4 = new Ss2();
            ss1.S.Send(0);
            ss1.S.Send(1);
            ss1.S.Send(2);
            bss.Send(ss2);
            ss1.S.Send(7);
            ss2.S.Send(3);
            ss2.S.Send(4);
            ss3.S.Send(2);
            bss.Send(ss3);
            ss3.S.Send(5);
            ss3.S.Send(6);
            ss3.S.Send(7);
            Transaction.RunVoid(() =>
            {
                ss3.S.Send(8);
                bss.Send(ss4);
                ss4.S.Send(2);
            });
            ss4.S.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, @out);
        }

        [Test]
        public void TestLiftList()
        {
            IReadOnlyList<CellSink<int>> cellSinks = Enumerable.Range(0, 50).Select(_ => Cell.CreateSink(1)).ToArray();
            Cell<int> sum = cellSinks.Lift().Map(v => v.Sum());
            List<int> @out = new List<int>();
            IListener l = sum.Listen(@out.Add);
            cellSinks[4].Send(5);
            cellSinks[5].Send(5);
            Transaction.RunVoid(() =>
            {
                cellSinks[9].Send(5);
                cellSinks[17].Send(5);
                cellSinks[41].Send(5);
                cellSinks[48].Send(5);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 50, 54, 58, 74 }, @out);
        }

        [Test]
        public void TestLiftLoopList()
        {
            (Cell<int> c, IReadOnlyList<CellSink<int>> s) = Transaction.Run(() =>
            {
                IReadOnlyList<CellLoop<int>> cellLoops = Enumerable.Range(0, 50).Select(_ => Cell.CreateLoop<int>()).ToArray();
                Cell<int> sum = cellLoops.Lift().Map(v => v.Sum());
                IReadOnlyList<CellSink<int>> cellSinks = Enumerable.Range(0, 50).Select(_ => Cell.CreateSink(1)).ToArray();
                for (int i = 0; i < 50; i++)
                {
                    cellLoops[i].Loop(cellSinks[i]);
                }
                return (sum, cellSinks);
            });
            List<int> @out = new List<int>();
            IListener l = c.Listen(@out.Add);
            s[4].Send(5);
            s[5].Send(5);
            Transaction.RunVoid(() =>
            {
                s[9].Send(5);
                s[17].Send(5);
                s[41].Send(5);
                s[48].Send(5);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 50, 54, 58, 74 }, @out);
        }

        [Test]
        public void TestLiftListLarge()
        {
            IReadOnlyList<CellSink<int>> cellSinks = Enumerable.Range(0, 500).Select(_ => Cell.CreateSink(1)).ToArray();
            Cell<int> sum = cellSinks.Lift().Map(v => v.Sum());
            List<int> @out = new List<int>();
            IListener l = sum.Listen(@out.Add);
            cellSinks[4].Send(5);
            cellSinks[5].Send(5);
            Transaction.RunVoid(() =>
            {
                cellSinks[9].Send(5);
                cellSinks[17].Send(5);
                cellSinks[41].Send(5);
                cellSinks[48].Send(5);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 500, 504, 508, 524 }, @out);
        }

        [Test]
        public void TestLiftListLargeManyUpdates()
        {
            IReadOnlyList<CellSink<int>> cellSinks = Enumerable.Range(0, 500).Select(_ => Cell.CreateSink(1)).ToArray();
            Cell<int> sum = cellSinks.Lift().Map(v => v.Sum());
            List<int> @out = new List<int>();
            IListener l = sum.Listen(@out.Add);
            for (int i = 0; i < 100; i++)
            {
                int n = i;
                cellSinks[n * 5].Send(5);
                cellSinks[n * 5 + 1].Send(5);
                Transaction.RunVoid(() =>
                {
                    cellSinks[n * 5 + 2].Send(5);
                    cellSinks[n * 5 + 3].Send(5);
                    cellSinks[n * 5 + 4].Send(5);
                });
            }
            l.Unlisten();
            IReadOnlyList<int> expected = new[] { 500 }.Concat(Enumerable.Range(0, 100).SelectMany(n => new[] { 500 + 20 * n + 4, 500 + 20 * n + 8, 500 + 20 * n + 20 })).ToArray();
            CollectionAssert.AreEqual(expected, @out);
        }

        [Test]
        public void TestLiftListChangesWhileListening()
        {
            IReadOnlyList<CellSink<int>> cellSinks = Enumerable.Range(0, 50).Select(_ => Cell.CreateSink(1)).ToArray();
            Cell<int> sum = cellSinks.Lift().Map(v => v.Sum());
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() =>
            {
                cellSinks[4].Send(5);
                IListener lLocal = sum.Listen(@out.Add);
                cellSinks[5].Send(5);
                return lLocal;
            });
            cellSinks[9].Send(5);
            Transaction.RunVoid(() =>
            {
                cellSinks[17].Send(5);
                cellSinks[41].Send(5);
                cellSinks[48].Send(5);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 58, 62, 74 }, @out);
        }

        [Test]
        public void SwitchCOnCellLoop()
        {
            (Cell<int> c, CellSink<int> c1, CellSink<int> c2, CellSink<Cell<int>> s) = Transaction.Run(() =>
            {
                CellLoop<Cell<int>> loop = Cell.CreateLoop<Cell<int>>();
                CellSink<int> c1Local = Cell.CreateSink(1);
                CellSink<int> c2Local = Cell.CreateSink(11);
                Cell<int> cLocal = loop.SwitchC();
                CellSink<Cell<int>> sLocal = Cell.CreateSink(c1Local.AsCell());
                loop.Loop(sLocal);
                return (cLocal, c1Local, c2Local, sLocal);
            });

            List<int> output = new List<int>();
            IListener l = c.Listen(output.Add);

            c1.Send(2);
            c2.Send(12);

            Transaction.RunVoid(() =>
            {
                c1.Send(3);
                c2.Send(13);
                s.Send(c2);
            });

            c1.Send(4);
            c2.Send(14);

            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 1, 2, 13, 14 }, output);
        }

        [Test]
        public void SwitchSOnBehaviorLoop()
        {
            (Stream<int> b, StreamSink<int> b1, StreamSink<int> b2, BehaviorSink<Stream<int>> s) = Transaction.Run(() =>
            {
                BehaviorLoop<Stream<int>> loop = Behavior.CreateLoop<Stream<int>>();
                StreamSink<int> b1Local = Stream.CreateSink<int>();
                StreamSink<int> b2Local = Stream.CreateSink<int>();
                Stream<int> bLocal = loop.SwitchS();
                BehaviorSink<Stream<int>> sLocal = Behavior.CreateSink(b1Local.AsStream());
                loop.Loop(sLocal);
                return (bLocal, b1Local, b2Local, sLocal);
            });

            List<int> output = new List<int>();
            IListener l = b.Listen(output.Add);

            b1.Send(2);
            b2.Send(12);

            Transaction.RunVoid(() =>
            {
                b1.Send(3);
                b2.Send(13);
                s.Send(b2);
            });

            b1.Send(4);
            b2.Send(14);

            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 2, 3, 14 }, output);
        }

        [Test]
        public void SwitchCCatchFirst()
        {
            List<int> output = new List<int>();

            (CellSink<int> c1, CellSink<int> c2, CellSink<Cell<int>> s, IListener l) = Transaction.Run(() =>
            {
                CellSink<int> c1Local = Cell.CreateSink(1);
                CellSink<int> c2Local = Cell.CreateSink(11);
                CellSink<Cell<int>> sLocal = Cell.CreateSink(c1Local.AsCell());
                Cell<int> cLocal = sLocal.SwitchC();

                c1Local.Send(2);
                c2Local.Send(12);
                sLocal.Send(c2Local);

                IListener lLocal = cLocal.Listen(output.Add);

                return (c1Local, c2Local, sLocal, lLocal);
            });

            c1.Send(3);
            c2.Send(13);

            Transaction.RunVoid(() =>
            {
                c1.Send(4);
                c2.Send(14);
                s.Send(c1);
            });

            c1.Send(5);
            c2.Send(15);

            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 12, 13, 4, 5 }, output);
        }

        [Test]
        public void SwitchSCatchFirst()
        {
            List<int> output = new List<int>();

            (StreamSink<int> c1, StreamSink<int> c2, BehaviorSink<Stream<int>> s, IListener l) = Transaction.Run(() =>
            {
                StreamSink<int> c1Local = Stream.CreateSink<int>();
                StreamSink<int> c2Local = Stream.CreateSink<int>();
                BehaviorSink<Stream<int>> sLocal = Behavior.CreateSink(c1Local.AsStream());
                Stream<int> cLocal = sLocal.SwitchS();

                c1Local.Send(2);
                c2Local.Send(12);
                sLocal.Send(c2Local);

                IListener lLocal = cLocal.Listen(output.Add);

                return (c1Local, c2Local, sLocal, lLocal);
            });

            c1.Send(3);
            c2.Send(13);

            Transaction.RunVoid(() =>
            {
                c1.Send(4);
                c2.Send(14);
                s.Send(c1);
            });

            c1.Send(5);
            c2.Send(15);

            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 2, 13, 14, 5 }, output);
        }

        [Test]
        public void SwitchSCatchFirstBefore()
        {
            List<int> output = new List<int>();

            (StreamSink<int> c1, StreamSink<int> c2, BehaviorSink<Stream<int>> s, IListener l) = Transaction.Run(() =>
            {
                StreamSink<int> c1Local = Stream.CreateSink<int>();
                StreamSink<int> c2Local = Stream.CreateSink<int>();
                BehaviorSink<Stream<int>> sLocal = Behavior.CreateSink(c1Local.AsStream());

                c1Local.Send(2);
                c2Local.Send(12);
                sLocal.Send(c2Local);

                Stream<int> cLocal = sLocal.SwitchS();

                IListener lLocal = cLocal.Listen(output.Add);

                return (c1Local, c2Local, sLocal, lLocal);
            });

            c1.Send(3);
            c2.Send(13);

            Transaction.RunVoid(() =>
            {
                c1.Send(4);
                c2.Send(14);
                s.Send(c1);
            });

            c1.Send(5);
            c2.Send(15);

            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 2, 13, 14, 5 }, output);
        }

        [Test]
        public void TestLiftInSwitchC()
        {
            IReadOnlyList<Test> list1 = new[] { new Test(0), new Test(1), new Test(2), new Test(3), new Test(4) };
            IReadOnlyList<Test> list2 = new[] { new Test(5), new Test(6), new Test(7), new Test(8), new Test(9) };

            BehaviorSink<IReadOnlyList<Test>> v = Behavior.CreateSink(list1);

            Behavior<IReadOnlyList<int>> c = v.Map(oo => oo.Select(o => o.Value).Lift()).SwitchB();

            List<IReadOnlyList<int>> streamOutput = new List<IReadOnlyList<int>>();
            IListener l = Operational.Updates(c).Listen(streamOutput.Add);

            List<IReadOnlyList<int>> behaviorOutput = new List<IReadOnlyList<int>>();
            IListener l2 = Transaction.Run(() => Operational.Value(c).Listen(behaviorOutput.Add));

            list1[2].Value.Send(12);
            list2[1].Value.Send(16);
            list1[4].Value.Send(14);
            Transaction.RunVoid(() =>
            {
                list2[2].Value.Send(17);
                list1[0].Value.Send(10);
                v.Send(list2);
            });
            list1[3].Value.Send(13);
            list2[3].Value.Send(18);

            l2.Unlisten();
            l.Unlisten();

            Assert.AreEqual(4, streamOutput.Count);
            Assert.AreEqual(5, behaviorOutput.Count);

            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4 }, behaviorOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, streamOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, behaviorOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, streamOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, behaviorOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, streamOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, behaviorOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, streamOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, behaviorOutput[4]);
        }

        [Test]
        public void TestMapWithSwitchC()
        {
            IReadOnlyList<Test> list1 = new[] { new Test(0), new Test(1), new Test(2), new Test(3), new Test(4) };
            IReadOnlyList<Test> list2 = new[] { new Test(5), new Test(6), new Test(7), new Test(8), new Test(9) };

            BehaviorSink<IReadOnlyList<Test>> v = Behavior.CreateSink(list1);

            Behavior<IReadOnlyList<int>> c = v.Map(oo => oo.Select(o => o.Value).Lift()).Map(o => o).SwitchB();

            List<IReadOnlyList<int>> streamOutput = new List<IReadOnlyList<int>>();
            IListener l = Operational.Updates(c).Listen(streamOutput.Add);

            List<IReadOnlyList<int>> behaviorOutput = new List<IReadOnlyList<int>>();
            IListener l2 = Transaction.Run(() => Operational.Value(c).Listen(behaviorOutput.Add));

            list1[2].Value.Send(12);
            list2[1].Value.Send(16);
            list1[4].Value.Send(14);
            Transaction.RunVoid(() =>
            {
                list2[2].Value.Send(17);
                list1[0].Value.Send(10);
                v.Send(list2);
            });
            list1[3].Value.Send(13);
            list2[3].Value.Send(18);

            l2.Unlisten();
            l.Unlisten();

            Assert.AreEqual(4, streamOutput.Count);
            Assert.AreEqual(5, behaviorOutput.Count);

            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4 }, behaviorOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, streamOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, behaviorOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, streamOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, behaviorOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, streamOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, behaviorOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, streamOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, behaviorOutput[4]);
        }

        public class Test
        {
            public Test(int initialValue)
            {
                this.Value = Behavior.CreateSink(initialValue);
            }

            public BehaviorSink<int> Value { get; }
        }
    }
}