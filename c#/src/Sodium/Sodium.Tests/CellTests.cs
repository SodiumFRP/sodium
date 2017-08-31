using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class CellTests
    {
        [Test]
        public void TestHold()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            DiscreteCell<int> c = s.Hold(0);
            List<int> @out = new List<int>();
            IListener l = c.Listen(@out.Add);
            s.Send(2);
            s.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 0, 2, 9 }, @out);
        }

        [Test]
        public void TestHoldUpdates()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            DiscreteCell<int> c = s.Hold(0);
            List<int> @out = new List<int>();
            IListener l = Operational.Updates(c.Cell).Listen(@out.Add);
            s.Send(2);
            s.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 9 }, @out);
        }

        [Test]
        public void TestSnapshot()
        {
            CellSink<int> c = Cell.CreateSink(0);
            StreamSink<long> trigger = Stream.CreateSink<long>();
            List<string> @out = new List<string>();
            IListener l = trigger.Snapshot(c, (x, y) => x + " " + y).Listen(@out.Add);
            trigger.Send(100L);
            c.Send(2);
            trigger.Send(200L);
            c.Send(9);
            c.Send(1);
            trigger.Send(300L);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "100 0", "200 2", "300 1" }, @out);
        }

        [Test]
        public void TestListen()
        {
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(9);
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
            IListener l = Transaction.Run(() => Operational.Value(c).ListenOnce(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9 }, @out);
        }

        [Test]
        public async Task TestListenOnceAsync()
        {
            CellSink<int> c = Cell.CreateSink(9);
            int result = await Transaction.Run(() => Operational.Value(c).ListenOnceAsync());
            c.Send(2);
            c.Send(7);
            Assert.AreEqual(9, result);
        }

        [Test]
        public void TestUpdates()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Operational.Updates(c).Listen(@out.Add);
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestDiscreteCellUpdates()
        {
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = c.Updates.Listen(@out.Add);
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestValue()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => Operational.Value(c).Listen(@out.Add));
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
                this.removeStreamSink = new StreamSink<Unit>();
                this.RemoveStream = this.removeStreamSink;

                this.changeNumber1StreamSink = new StreamSink<int>();
                this.Number1Cell = this.changeNumber1StreamSink.Hold(n1);
                this.changeNumber2StreamSink = new StreamSink<int>();
                this.Number2Cell = this.changeNumber2StreamSink.Hold(n2);
                this.BothNumbersCell = this.Number1Cell.Lift(this.Number2Cell, Tuple.Create).Values.Hold(Tuple.Create(99, 99));
            }

            public Stream<Unit> RemoveStream { get; }
            public DiscreteCell<int> Number1Cell { get; }
            public DiscreteCell<int> Number2Cell { get; }
            public DiscreteCell<Tuple<int, int>> BothNumbersCell { get; }

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
        public void TestDiscreteCellLoopComplex()
        {
            StreamSink<int> s = new StreamSink<int>();
            StreamSink<Tuple<int, int>> addItemStreamSink = new StreamSink<Tuple<int, int>>();
            StreamSink<IReadOnlyList<TestObject>> removeItemsStreamSink = new StreamSink<IReadOnlyList<TestObject>>();
            DiscreteCell<IReadOnlyList<TestObject>> listCell = Transaction.Run(() =>
            {
                DiscreteCellLoop<IReadOnlyList<TestObject>> listCellLoop = new DiscreteCellLoop<IReadOnlyList<TestObject>>();
                DiscreteCell<IReadOnlyList<TestObject>> listCellLocal =
                    addItemStreamSink.OrElse(s.Map(v => Tuple.Create(v, v))).Map<Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>>(o => c => c.Concat(new[] { new TestObject(o.Item1, o.Item2) }).ToArray())
                        .Merge(removeItemsStreamSink.Map<Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>>(o => c => c.Except(o).ToArray()), (f, g) => c => g(f(c)))
                        .Snapshot(listCellLoop, (f, c) => f(c))
                        .Hold(new TestObject[0]);
                listCellLoop.Loop(listCellLocal);
                return listCellLocal;
            });
            IListener l2 = Transaction.Run(() =>
            {
                return listCell.Values.Listen(c => Transaction.Post(() =>
                {
                    if (c.Any())
                    {
                        Tuple<int, int> t = c.Last().BothNumbersCell.Cell.Sample();
                        if (t.Item1 == 9 && t.Item2 == 9)
                        {
                            addItemStreamSink.Send(Tuple.Create(0, 0));
                        }
                    }
                }));
            });
            IListener l3 = Transaction.Run(() =>
                listCell.Map(c => c.Select(o => o.RemoveStream.MapTo(new[] { o })).Merge((x, y) => x.Concat(y).ToArray())).SwitchS().Listen(o => Transaction.Post(() => removeItemsStreamSink.Send(o))));
            IListener l4 = Transaction.Run(() =>
                listCell.Map(c => c.Any() ? c.Last().Number1Cell.Lift(c.Last().Number2Cell, (x, y) => x == 9 && y == 9).Updates : Stream.Never<bool>()).SwitchS().Filter(v => v).Listen(_ => Transaction.Post(() => addItemStreamSink.Send(Tuple.Create(0, 0)))));
            List<IReadOnlyList<Tuple<int, int>>> @out = new List<IReadOnlyList<Tuple<int, int>>>();
            IListener l = listCell.Map(c => c.Select(o => o.Number1Cell.Lift(o.Number2Cell, Tuple.Create)).Lift()).SwitchC().Listen(@out.Add);
            addItemStreamSink.Send(Tuple.Create(5, 2));
            addItemStreamSink.Send(Tuple.Create(9, 2));
            listCell.Cell.Sample()[0].Remove();
            addItemStreamSink.Send(Tuple.Create(2, 9));
            listCell.Cell.Sample()[1].ChangeNumber1(9);
            addItemStreamSink.Send(Tuple.Create(9, 9));
            s.Send(5);
            s.Send(9);
            Transaction.RunVoid(() =>
            {
                addItemStreamSink.Send(Tuple.Create(5, 5));
                s.Send(5);
            });
            listCell.Cell.Sample()[8].ChangeNumber2(9);
            listCell.Cell.Sample()[8].ChangeNumber1(9);
            l.Unlisten();
            l2.Unlisten();
            l3.Unlisten();
            l4.Unlisten();

            Tuple<int, int>[][] expected =
            {
                new Tuple<int, int>[0],
                new[] { Tuple.Create(5, 2) },
                new[] { Tuple.Create(5, 2), Tuple.Create(9, 2) },
                new[] { Tuple.Create(9, 2) },
                new[] { Tuple.Create(9, 2), Tuple.Create(2, 9) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9), Tuple.Create(0, 0) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9), Tuple.Create(0, 0) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(5, 5) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(5, 5), Tuple.Create(9, 9) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(5, 5), Tuple.Create(9, 9), Tuple.Create(0, 0) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(5, 5), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(5, 5) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(5, 5), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(5, 9) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(5, 5), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9) },
                new[] { Tuple.Create(9, 2), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(5, 5), Tuple.Create(9, 9), Tuple.Create(0, 0), Tuple.Create(9, 9), Tuple.Create(0, 0) }
            };
            Assert.AreEqual(expected.Length, @out.Count);
            for (int i = 0; i < 16; i++)
            {
                CollectionAssert.AreEqual(@out[i], expected[i]);
            }
        }

        [Test]
        public void TestDiscreteCellLoop()
        {
            StreamSink<int> s = new StreamSink<int>();
            DiscreteCell<int> cell = Transaction.Run(() =>
            {
                DiscreteCellLoop<int> cellLoop = new DiscreteCellLoop<int>();
                DiscreteCell<int> cellLocal = s.Snapshot(cellLoop, (x, y) => x + y).Hold(1);
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
        public void TestDiscreteCellLoopThrowsException()
        {
            //TODO: adjust the types so that loops can only be created safely through the type system

            Exception actual = null;

            try
            {
                StreamSink<int> s = new StreamSink<int>();
                DiscreteCell<int> cell = Transaction.Run(() =>
                {
                    DiscreteCellLoop<int> cellLoop = new DiscreteCellLoop<int>();
                    DiscreteCell<int> cellLocal = cellLoop.Updates.Filter(v => v % 2 == 0).Map(v => v + 1).Merge(s, (_, r) => r).Hold(1);
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
            catch (Exception e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("A dependency cycle was detected.", actual.Message);
        }

        [Test]
        public void TestStreamLoop()
        {
            StreamSink<int> streamSink = new StreamSink<int>();
            Stream<int> s = Transaction.Run(() =>
            {
                StreamLoop<int> sl = new StreamLoop<int>();
                DiscreteCell<int> c = sl.Map(v => v + 2).Hold(0);
                Stream<int> s2 = streamSink.Snapshot(c, (x, y) => x + y);
                sl.Loop(s2);
                return s2;
            });
            List<int> @out = new List<int>();
            IListener l = s.Listen(@out.Add);
            streamSink.Send(3);
            streamSink.Send(4);
            streamSink.Send(7);
            streamSink.Send(8);
            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 3, 9, 18, 28 }, @out);
        }

        [Test]
        public void TestDiscreteCellValues()
        {
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Values.Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9, 2, 7 }, @out);
        }

        [Test]
        public void TestDiscreteCellValuesNoTransaction()
        {
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = c.Values.Listen(@out.Add);
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestValueThenMap()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => Operational.Value(c).Map(x => x + 100).Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 109, 102, 107 }, @out);
        }

        [Test]
        public void TestDiscreteCellValuesThenMap()
        {
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Values.Map(x => x + 100).Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 109, 102, 107 }, @out);
        }

        [Test]
        public void TestValueThenMerge()
        {
            CellSink<int> c1 = Cell.CreateSink(9);
            CellSink<int> c2 = Cell.CreateSink(2);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => Operational.Value(c1).Merge(Operational.Value(c2), (x, y) => x + y).Listen(@out.Add));
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
        public void TestDiscreteCellValuesThenMerge()
        {
            DiscreteCellSink<int> c1 = DiscreteCell.CreateSink(9);
            DiscreteCellSink<int> c2 = DiscreteCell.CreateSink(2);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c1.Values.Merge(c2.Values, (x, y) => x + y).Listen(@out.Add));
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
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => Operational.Value(c).Filter(x => x % 2 != 0).Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9, 7 }, @out);
        }

        [Test]
        public void TestDiscreteCellValuesThenFilter()
        {
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Values.Filter(x => x % 2 != 0).Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9, 7 }, @out);
        }

        [Test]
        public void TestValueThenOnce()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => Operational.Value(c).Once().Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9 }, @out);
        }

        [Test]
        public void TestDiscreteCellValuesThenOnce()
        {
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(9);
            List<int> @out = new List<int>();
            IListener l = Transaction.Run(() => c.Values.Once().Listen(@out.Add));
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 9 }, @out);
        }

        [Test]
        public void TestValueThenLateListen()
        {
            CellSink<int> c = Cell.CreateSink(9);
            List<int> @out = new List<int>();
            Stream<int> value = Operational.Value(c);
            c.Send(8);
            IListener l = value.Listen(@out.Add);
            c.Send(2);
            c.Send(7);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestDiscreteCellValuesThenLateListen()
        {
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(9);
            List<int> @out = new List<int>();
            Stream<int> value = c.Values;
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
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(6);
            List<string> @out = new List<string>();
            IListener l = c.Map(x => x.ToString()).Listen(@out.Add);
            c.Send(8);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "6", "8" }, @out);
        }

        [Test]
        public void TestMapLateListen()
        {
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(6);
            List<string> @out = new List<string>();
            DiscreteCell<string> cm = c.Map(x => x.ToString());
            c.Send(2);
            IListener l = cm.Listen(@out.Add);
            c.Send(8);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "2", "8" }, @out);
        }

        [Test]
        public void TestCalm()
        {
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(2);
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
            DiscreteCellSink<int> c = DiscreteCell.CreateSink(2);
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
            DiscreteCellSink<Func<long, string>> cf = DiscreteCell.CreateSink<Func<long, string>>(x => "1 " + x);
            DiscreteCellSink<long> ca = DiscreteCell.CreateSink(5L);
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
            DiscreteCellSink<int> c1 = DiscreteCell.CreateSink(1);
            DiscreteCellSink<long> c2 = DiscreteCell.CreateSink(5L);
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
            DiscreteCellSink<int> c1 = DiscreteCell.CreateSink(1);
            DiscreteCell<int> c3 = c1.Map(x => x * 3);
            DiscreteCell<int> c5 = c1.Map(x => x * 5);
            DiscreteCell<string> c = c3.Lift(c5, (x, y) => x + " " + y);
            List<string> @out = new List<string>();
            IListener l = c.Listen(@out.Add);
            c1.Send(2);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "3 5", "6 10" }, @out);
        }

        [Test]
        public void TestLiftFromSimultaneous()
        {
            ValueTuple<DiscreteCellSink<int>, DiscreteCellSink<int>> t = Transaction.Run(() =>
            {
                DiscreteCellSink<int> localC1 = DiscreteCell.CreateSink(3);
                DiscreteCellSink<int> localC2 = DiscreteCell.CreateSink(5);
                localC2.Send(7);
                return ValueTuple.Create(localC1, localC2);
            });
            DiscreteCellSink<int> c1 = t.Item1;
            DiscreteCellSink<int> c2 = t.Item2;
            List<int> @out = new List<int>();
            IListener l = c1.Lift(c2, (x, y) => x + y).Listen(@out.Add);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 10 }, @out);
        }

        [Test]
        public void TestHoldIsDelayed()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            DiscreteCell<int> h = s.Hold(0);
            Stream<string> pair = s.Snapshot(h.Cell, (a, b) => a + " " + b);
            List<string> @out = new List<string>();
            IListener l = pair.Listen(@out.Add);
            s.Send(2);
            s.Send(3);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "2 0", "3 2" }, @out);
        }

        private class Sc
        {
            public readonly IMaybe<char> A;
            public readonly IMaybe<char> B;
            public readonly IMaybe<DiscreteCell<char>> Sw;

            public Sc(IMaybe<char> a, IMaybe<char> b, IMaybe<DiscreteCell<char>> sw)
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
            DiscreteCell<char> ca = ssc.Map(s => s.A).FilterMaybe().Hold('A');
            DiscreteCell<char> cb = ssc.Map(s => s.B).FilterMaybe().Hold('a');
            DiscreteCell<DiscreteCell<char>> csw = ssc.Map(s => s.Sw).FilterMaybe().Hold(ca);
            DiscreteCell<char> co = csw.SwitchC();
            List<char> @out = new List<char>();
            IListener l = co.Listen(@out.Add);
            ssc.Send(new Sc(Maybe.Just('B'), Maybe.Just('b'), Maybe.Nothing<DiscreteCell<char>>()));
            ssc.Send(new Sc(Maybe.Just('C'), Maybe.Just('c'), Maybe.Just(cb)));
            ssc.Send(new Sc(Maybe.Just('D'), Maybe.Just('d'), Maybe.Nothing<DiscreteCell<char>>()));
            ssc.Send(new Sc(Maybe.Just('E'), Maybe.Just('e'), Maybe.Just(ca)));
            ssc.Send(new Sc(Maybe.Just('F'), Maybe.Just('f'), Maybe.Nothing<DiscreteCell<char>>()));
            ssc.Send(new Sc(Maybe.Nothing<char>(), Maybe.Nothing<char>(), Maybe.Just(cb)));
            ssc.Send(new Sc(Maybe.Nothing<char>(), Maybe.Nothing<char>(), Maybe.Just(ca)));
            ssc.Send(new Sc(Maybe.Just('G'), Maybe.Just('g'), Maybe.Just(cb)));
            ssc.Send(new Sc(Maybe.Just('H'), Maybe.Just('h'), Maybe.Just(ca)));
            ssc.Send(new Sc(Maybe.Just('I'), Maybe.Just('i'), Maybe.Just(ca)));
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 'A', 'B', 'c', 'd', 'E', 'F', 'f', 'F', 'g', 'H', 'I' }, @out);
        }

        private class Sc2
        {
            public readonly DiscreteCellSink<int> C;

            public Sc2(int initialValue)
            {
                this.C = DiscreteCell.CreateSink(initialValue);
            }
        }

        [Test]
        public void TestSwitchCSimultaneous()
        {
            Sc2 sc1 = new Sc2(0);
            DiscreteCellSink<Sc2> csc = DiscreteCell.CreateSink(sc1);
            DiscreteCell<int> co = csc.Map<DiscreteCell<int>>(b => b.C).SwitchC();
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
            public readonly IMaybe<Stream<char>> Sw;

            public Ss(char a, char b, IMaybe<Stream<char>> sw)
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
            DiscreteCell<Stream<char>> csw = sss.Map(s => s.Sw).FilterMaybe().Hold(sa);
            Stream<char> so = csw.Cell.SwitchS();
            List<char> @out = new List<char>();
            IListener l = so.Listen(@out.Add);
            sss.Send(new Ss('A', 'a', Maybe.Nothing<Stream<char>>()));
            sss.Send(new Ss('B', 'b', Maybe.Nothing<Stream<char>>()));
            sss.Send(new Ss('C', 'c', Maybe.Just(sb)));
            sss.Send(new Ss('D', 'd', Maybe.Nothing<Stream<char>>()));
            sss.Send(new Ss('E', 'e', Maybe.Just(sa)));
            sss.Send(new Ss('F', 'f', Maybe.Nothing<Stream<char>>()));
            sss.Send(new Ss('G', 'g', Maybe.Just(sb)));
            sss.Send(new Ss('H', 'h', Maybe.Just(sa)));
            sss.Send(new Ss('I', 'i', Maybe.Just(sa)));
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
            CellSink<Ss2> css = Cell.CreateSink(ss1);
            Stream<int> so = css.Map<Stream<int>>(b => b.S).SwitchS();
            List<int> @out = new List<int>();
            IListener l = so.Listen(@out.Add);
            Ss2 ss2 = new Ss2();
            Ss2 ss3 = new Ss2();
            Ss2 ss4 = new Ss2();
            ss1.S.Send(0);
            ss1.S.Send(1);
            ss1.S.Send(2);
            css.Send(ss2);
            ss1.S.Send(7);
            ss2.S.Send(3);
            ss2.S.Send(4);
            ss3.S.Send(2);
            css.Send(ss3);
            ss3.S.Send(5);
            ss3.S.Send(6);
            ss3.S.Send(7);
            Transaction.RunVoid(() =>
            {
                ss3.S.Send(8);
                css.Send(ss4);
                ss4.S.Send(2);
            });
            ss4.S.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, @out);
        }

        [Test]
        public void TestSwitchEarlyS()
        {
            Ss2 ss1 = new Ss2();
            CellSink<Ss2> css = Cell.CreateSink(ss1);
            Stream<int> so = css.Map<Stream<int>>(b => b.S).SwitchEarlyS();
            List<int> @out = new List<int>();
            IListener l = so.Listen(@out.Add);
            Ss2 ss2 = new Ss2();
            Ss2 ss3 = new Ss2();
            Ss2 ss4 = new Ss2();
            ss1.S.Send(0);
            ss1.S.Send(1);
            ss1.S.Send(2);
            css.Send(ss2);
            ss1.S.Send(7);
            ss2.S.Send(3);
            ss2.S.Send(4);
            ss3.S.Send(2);
            css.Send(ss3);
            ss3.S.Send(5);
            ss3.S.Send(6);
            ss3.S.Send(7);
            ss4.S.Send(8);
            css.Send(ss4);
            ss4.S.Send(8);
            ss3.S.Send(2);
            ss4.S.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, @out);
        }

        [Test]
        public void TestSwitchEarlySSimultaneous()
        {
            Ss2 ss1 = new Ss2();
            CellSink<Ss2> css = Cell.CreateSink(ss1);
            Stream<int> so = css.Map<Stream<int>>(b => b.S).SwitchEarlyS();
            List<int> @out = new List<int>();
            IListener l = so.Listen(@out.Add);
            Ss2 ss2 = new Ss2();
            Ss2 ss3 = new Ss2();
            Ss2 ss4 = new Ss2();
            ss1.S.Send(0);
            ss1.S.Send(1);
            ss1.S.Send(2);
            css.Send(ss2);
            ss1.S.Send(7);
            ss2.S.Send(3);
            ss2.S.Send(4);
            ss3.S.Send(2);
            css.Send(ss3);
            ss3.S.Send(5);
            ss3.S.Send(6);
            ss3.S.Send(7);
            Transaction.RunVoid(() =>
            {
                ss4.S.Send(8);
                css.Send(ss4);
                ss3.S.Send(2);
            });
            ss4.S.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, @out);
        }

        [Test]
        public void TestLiftList()
        {
            IReadOnlyList<DiscreteCellSink<int>> cellSinks = Enumerable.Range(0, 50).Select(_ => DiscreteCell.CreateSink(1)).ToArray();
            DiscreteCell<int> sum = cellSinks.Lift().Map(v => v.Sum());
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
            ValueTuple<DiscreteCell<int>, IReadOnlyList<DiscreteCellSink<int>>> t = Transaction.Run(() =>
            {
                IReadOnlyList<DiscreteCellLoop<int>> cellLoops = Enumerable.Range(0, 50).Select(_ => DiscreteCell.CreateLoop<int>()).ToArray();
                DiscreteCell<int> sum = cellLoops.Lift().Map(v => v.Sum());
                IReadOnlyList<DiscreteCellSink<int>> cellSinks = Enumerable.Range(0, 50).Select(_ => DiscreteCell.CreateSink(1)).ToArray();
                for (int i = 0; i < 50; i++)
                {
                    cellLoops[i].Loop(cellSinks[i]);
                }
                return ValueTuple.Create(sum, cellSinks);
            });
            List<int> @out = new List<int>();
            IListener l = t.Item1.Listen(@out.Add);
            t.Item2[4].Send(5);
            t.Item2[5].Send(5);
            Transaction.RunVoid(() =>
            {
                t.Item2[9].Send(5);
                t.Item2[17].Send(5);
                t.Item2[41].Send(5);
                t.Item2[48].Send(5);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 50, 54, 58, 74 }, @out);
        }

        [Test]
        public void TestLiftListLarge()
        {
            IReadOnlyList<DiscreteCellSink<int>> cellSinks = Enumerable.Range(0, 500).Select(_ => DiscreteCell.CreateSink(1)).ToArray();
            DiscreteCell<int> sum = cellSinks.Lift().Map(v => v.Sum());
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
            IReadOnlyList<DiscreteCellSink<int>> cellSinks = Enumerable.Range(0, 500).Select(_ => DiscreteCell.CreateSink(1)).ToArray();
            DiscreteCell<int> sum = cellSinks.Lift().Map(v => v.Sum());
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
            IReadOnlyList<DiscreteCellSink<int>> cellSinks = Enumerable.Range(0, 50).Select(_ => DiscreteCell.CreateSink(1)).ToArray();
            DiscreteCell<int> sum = cellSinks.Lift().Map(v => v.Sum());
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
            ValueTuple<DiscreteCell<int>, DiscreteCellSink<int>, DiscreteCellSink<int>, DiscreteCellSink<DiscreteCell<int>>> t = Transaction.Run(() =>
            {
                DiscreteCellLoop<DiscreteCell<int>> loop = DiscreteCell.CreateLoop<DiscreteCell<int>>();
                DiscreteCellSink<int> c1 = DiscreteCell.CreateSink(1);
                DiscreteCellSink<int> c2 = DiscreteCell.CreateSink(11);
                DiscreteCell<int> c = loop.SwitchC();
                DiscreteCellSink<DiscreteCell<int>> s = DiscreteCell.CreateSink(c1.AsDiscreteCell());
                loop.Loop(s);
                return ValueTuple.Create(c, c1, c2, s);
            });

            List<int> output = new List<int>();
            IListener l = t.Item1.Listen(output.Add);

            t.Item2.Send(2);
            t.Item3.Send(12);

            Transaction.RunVoid(() =>
            {
                t.Item2.Send(3);
                t.Item3.Send(13);
                t.Item4.Send(t.Item3);
            });

            t.Item2.Send(4);
            t.Item3.Send(14);

            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 1, 2, 13, 14 }, output);
        }

        [Test]
        public void SwitchSOnCellLoop()
        {
            ValueTuple<Stream<int>, StreamSink<int>, StreamSink<int>, CellSink<Stream<int>>> t = Transaction.Run(() =>
            {
                CellLoop<Stream<int>> loop = Cell.CreateLoop<Stream<int>>();
                StreamSink<int> c1 = Stream.CreateSink<int>();
                StreamSink<int> c2 = Stream.CreateSink<int>();
                Stream<int> c = loop.SwitchS();
                CellSink<Stream<int>> s = Cell.CreateSink(c1.AsStream());
                loop.Loop(s);
                return ValueTuple.Create(c, c1, c2, s);
            });

            List<int> output = new List<int>();
            IListener l = t.Item1.Listen(output.Add);

            t.Item2.Send(2);
            t.Item3.Send(12);

            Transaction.RunVoid(() =>
            {
                t.Item2.Send(3);
                t.Item3.Send(13);
                t.Item4.Send(t.Item3);
            });

            t.Item2.Send(4);
            t.Item3.Send(14);

            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 2, 3, 14 }, output);
        }

        [Test]
        public void SwitchEarlySOnCellLoop()
        {
            ValueTuple<Stream<int>, StreamSink<int>, StreamSink<int>, CellSink<Stream<int>>> t = Transaction.Run(() =>
            {
                CellLoop<Stream<int>> loop = Cell.CreateLoop<Stream<int>>();
                StreamSink<int> c1 = Stream.CreateSink<int>();
                StreamSink<int> c2 = Stream.CreateSink<int>();
                Stream<int> c = loop.SwitchEarlyS();
                CellSink<Stream<int>> s = Cell.CreateSink(c1.AsStream());
                loop.Loop(s);
                return ValueTuple.Create(c, c1, c2, s);
            });

            List<int> output = new List<int>();
            IListener l = t.Item1.Listen(output.Add);

            t.Item2.Send(2);
            t.Item3.Send(12);

            Transaction.RunVoid(() =>
            {
                t.Item2.Send(3);
                t.Item3.Send(13);
                t.Item4.Send(t.Item3);
            });

            t.Item2.Send(4);
            t.Item3.Send(14);

            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 2, 13, 14 }, output);
        }

        [Test]
        public void SwitchCCatchFirst()
        {
            List<int> output = new List<int>();

            ValueTuple<DiscreteCell<int>, DiscreteCellSink<int>, DiscreteCellSink<int>, DiscreteCellSink<DiscreteCell<int>>, IListener> t = Transaction.Run(() =>
            {
                DiscreteCellSink<int> c1 = DiscreteCell.CreateSink(1);
                DiscreteCellSink<int> c2 = DiscreteCell.CreateSink(11);
                DiscreteCellSink<DiscreteCell<int>> s = DiscreteCell.CreateSink(c1.AsDiscreteCell());
                DiscreteCell<int> c = s.SwitchC();

                c1.Send(2);
                c2.Send(12);
                s.Send(c2);

                IListener l = c.Listen(output.Add);

                return ValueTuple.Create(c, c1, c2, s, l);
            });

            t.Item2.Send(3);
            t.Item3.Send(13);

            Transaction.RunVoid(() =>
            {
                t.Item2.Send(4);
                t.Item3.Send(14);
                t.Item4.Send(t.Item2);
            });

            t.Item2.Send(5);
            t.Item3.Send(15);

            t.Item5.Unlisten();

            CollectionAssert.AreEqual(new[] { 12, 13, 4, 5 }, output);
        }

        [Test]
        public void SwitchSCatchFirst()
        {
            List<int> output = new List<int>();

            ValueTuple<Stream<int>, StreamSink<int>, StreamSink<int>, CellSink<Stream<int>>, IListener> t = Transaction.Run(() =>
            {
                StreamSink<int> c1 = Stream.CreateSink<int>();
                StreamSink<int> c2 = Stream.CreateSink<int>();
                CellSink<Stream<int>> s = Cell.CreateSink(c1.AsStream());
                Stream<int> c = s.SwitchS();

                c1.Send(2);
                c2.Send(12);
                s.Send(c2);

                IListener l = c.Listen(output.Add);

                return ValueTuple.Create(c, c1, c2, s, l);
            });

            t.Item2.Send(3);
            t.Item3.Send(13);

            Transaction.RunVoid(() =>
            {
                t.Item2.Send(4);
                t.Item3.Send(14);
                t.Item4.Send(t.Item2);
            });

            t.Item2.Send(5);
            t.Item3.Send(15);

            t.Item5.Unlisten();

            CollectionAssert.AreEqual(new[] { 2, 13, 14, 5 }, output);
        }

        [Test]
        public void SwitchEarlySCatchFirst()
        {
            List<int> output = new List<int>();

            ValueTuple<Stream<int>, StreamSink<int>, StreamSink<int>, CellSink<Stream<int>>, IListener> t = Transaction.Run(() =>
            {
                StreamSink<int> c1 = Stream.CreateSink<int>();
                StreamSink<int> c2 = Stream.CreateSink<int>();
                CellSink<Stream<int>> s = Cell.CreateSink(c1.AsStream());
                Stream<int> c = s.SwitchEarlyS();

                c1.Send(2);
                c2.Send(12);
                s.Send(c2);

                IListener l = c.Listen(output.Add);

                return ValueTuple.Create(c, c1, c2, s, l);
            });

            t.Item2.Send(3);
            t.Item3.Send(13);

            Transaction.RunVoid(() =>
            {
                t.Item2.Send(4);
                t.Item3.Send(14);
                t.Item4.Send(t.Item2);
            });

            t.Item2.Send(5);
            t.Item3.Send(15);

            t.Item5.Unlisten();

            CollectionAssert.AreEqual(new[] { 12, 13, 4, 5 }, output);
        }

        [Test]
        public void TestLiftInSwitchC()
        {
            IReadOnlyList<Test> list1 = new[] { new Test(0), new Test(1), new Test(2), new Test(3), new Test(4) };
            IReadOnlyList<Test> list2 = new[] { new Test(5), new Test(6), new Test(7), new Test(8), new Test(9) };

            CellSink<IReadOnlyList<Test>> v = Cell.CreateSink(list1);

            Cell<IReadOnlyList<int>> c = v.Map(oo => oo.Select(o => o.Value).Lift()).SwitchC();

            List<IReadOnlyList<int>> streamOutput = new List<IReadOnlyList<int>>();
            IListener l = Operational.Updates(c).Listen(streamOutput.Add);

            List<IReadOnlyList<int>> cellOutput = new List<IReadOnlyList<int>>();
            IListener l2 = Transaction.Run(() => Operational.Value(c).Listen(cellOutput.Add));

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
            Assert.AreEqual(5, cellOutput.Count);

            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4 }, cellOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, streamOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, cellOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, streamOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, cellOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, streamOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, cellOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, streamOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, cellOutput[4]);
        }

        [Test]
        public void TestMapWithSwitchC()
        {
            IReadOnlyList<Test> list1 = new[] { new Test(0), new Test(1), new Test(2), new Test(3), new Test(4) };
            IReadOnlyList<Test> list2 = new[] { new Test(5), new Test(6), new Test(7), new Test(8), new Test(9) };

            CellSink<IReadOnlyList<Test>> v = Cell.CreateSink(list1);

            Cell<IReadOnlyList<int>> c = v.Map(oo => oo.Select(o => o.Value).Lift()).Map(o => o).SwitchC();

            List<IReadOnlyList<int>> streamOutput = new List<IReadOnlyList<int>>();
            IListener l = Operational.Updates(c).Listen(streamOutput.Add);

            List<IReadOnlyList<int>> cellOutput = new List<IReadOnlyList<int>>();
            IListener l2 = Transaction.Run(() => Operational.Value(c).Listen(cellOutput.Add));

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
            Assert.AreEqual(5, cellOutput.Count);

            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4 }, cellOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, streamOutput[0]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 4 }, cellOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, streamOutput[1]);
            CollectionAssert.AreEqual(new[] { 0, 1, 12, 3, 14 }, cellOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, streamOutput[2]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 8, 9 }, cellOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, streamOutput[3]);
            CollectionAssert.AreEqual(new[] { 5, 16, 17, 18, 9 }, cellOutput[4]);
        }

        public class Test
        {
            public Test(int initialValue)
            {
                this.Value = Cell.CreateSink(initialValue);
            }

            public CellSink<int> Value { get; }
        }
    }
}