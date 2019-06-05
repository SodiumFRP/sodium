using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests
{
    [TestFixture]
    public class CellTests
    {
        [Test]
        public void TestLoop()
        {
            (Cell<int> c, CellStreamSink<int> s) = Transaction.Run(() =>
             {
                 CellLoop<int> loop = Cell.CreateLoop<int>();
                 Cell<int> cLocal = loop.Map(v => v * 5);
                 CellStreamSink<int> sLocal = Cell.CreateStreamSink<int>();
                 loop.Loop(sLocal.Hold(3));
                 return (cLocal, sLocal);
             });

            List<int> output1 = new List<int>();
            List<int> output2 = new List<int>();
            IListener l = c.Listen(output1.Add);
            IListener l2 = c.Updates().Listen(output2.Add);

            s.Send(5);
            s.Send(7);

            l2.Unlisten();
            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 15, 25, 35 }, output1);
            CollectionAssert.AreEqual(new[] { 25, 35 }, output2);
        }

        [Test]
        public void TestLiftSimultaneousUpdates()
        {
            List<int> @out = new List<int>();
            CellSink<int> cellSink = Cell.CreateSink(1);
            Cell<int> cell = cellSink.Map(v => 2 * v);
            IListener l = cellSink.Lift(cell, (x, y) => x + y).Updates().Listen(@out.Add);

            cellSink.Send(2);
            cellSink.Send(7);

            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 6, 21 }, @out);
        }

        [Test]
        public void TestLiftInSwitchC()
        {
            IReadOnlyList<Test> list1 = new[] { new Test(0), new Test(1), new Test(2), new Test(3), new Test(4) };
            IReadOnlyList<Test> list2 = new[] { new Test(5), new Test(6), new Test(7), new Test(8), new Test(9) };

            CellSink<IReadOnlyList<Test>> v = Cell.CreateSink(list1);

            Cell<IReadOnlyList<int>> c = v.Map(oo => oo.Select(o => o.Value).Lift()).SwitchC();

            List<IReadOnlyList<int>> streamOutput = new List<IReadOnlyList<int>>();
            IListener l = c.Updates().Listen(streamOutput.Add);

            List<IReadOnlyList<int>> cellOutput = new List<IReadOnlyList<int>>();
            IListener l2 = c.Listen(cellOutput.Add);

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
            IListener l = c.Updates().Listen(streamOutput.Add);

            List<IReadOnlyList<int>> cellOutput = new List<IReadOnlyList<int>>();
            IListener l2 = c.Listen(cellOutput.Add);

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

        [Test]
        public void TestLiftCellsInSwitchC()
        {
            List<int> @out = new List<int>();
            CellSink<int> s = Cell.CreateSink(0);
            Cell<Cell<int>> c = Cell.Constant(Cell.Constant(1));
            Cell<Cell<int>> r = c.Map(c2 => c2.Lift(s, (v1, v2) => v1 + v2));
            IListener l = r.SwitchC().Listen(@out.Add);
            s.Send(2);
            s.Send(4);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 1, 3, 5 }, @out);
        }

        [Test]
        public void TestLazyCellCreation()
        {
            List<int> @out = new List<int>();
            StreamSink<int> s = Stream.CreateSink<int>();
            Cell<Cell<int>> c = Cell.Constant(1).Map(_ => s.Hold(0));
            s.Send(1);
            IListener l = c.SwitchC().Listen(@out.Add);
            s.Send(3);
            s.Send(5);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 1, 3, 5 }, @out);
        }

        [Test]
        public void CellValuesWithPrevious()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            Cell<int> c = s.Hold(0);
            List<(int Current, Maybe<int> Previous)> @out = new List<(int Current, Maybe<int> Previous)>();
            using (Transaction.Run(
                () =>
                {
                    Stream<(int Current, Maybe<int> Previous)> r = c.Updates()
                        .Snapshot(c, (n, o) => (Current: n, Previous: Maybe.Some(o)))
                        .OrElse(
                            Cell.ConstantLazy(c.SampleLazy()).Values().Map(v => (Current: v, Previous: Maybe<int>.None)));
                    return r.Listen(@out.Add);
                }))
            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
                s.Send(4);
            }

            CollectionAssert.AreEqual(
                new[]
                {
                    (Current: 0, Previous: Maybe.None),
                    (Current: 1, Previous: Maybe.Some(0)),
                    (Current: 2, Previous: Maybe.Some(1)),
                    (Current: 3, Previous: Maybe.Some(2)),
                    (Current: 4, Previous: Maybe.Some(3))
                },
                @out);
        }

        [Test]
        public void CellValuesWithPreviousHavingInitialUpdate()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            Cell<int> c = s.Hold(0);
            List<(int Current, Maybe<int> Previous)> @out = new List<(int Current, Maybe<int> Previous)>();
            using (Transaction.Run(
                () =>
                {
                    Stream<(int Current, Maybe<int> Previous)> r = c.Updates()
                        .Snapshot(c, (n, o) => (Current: n, Previous: Maybe.Some(o)))
                        .OrElse(
                            Cell.ConstantLazy(c.SampleLazy()).Values().Map(v => (Current: v, Previous: Maybe<int>.None)));
                    s.Send(1);
                    return r.Listen(@out.Add);
                }))
            {
                s.Send(2);
                s.Send(3);
                s.Send(4);
                s.Send(5);
            }

            CollectionAssert.AreEqual(
                new[]
                {
                    (Current: 1, Previous: Maybe.Some(0)),
                    (Current: 2, Previous: Maybe.Some(1)),
                    (Current: 3, Previous: Maybe.Some(2)),
                    (Current: 4, Previous: Maybe.Some(3)),
                    (Current: 5, Previous: Maybe.Some(4))
                },
                @out);
        }

        [Test]
        public void TestLoopAndSwitchCError()
        {
            InvalidOperationException exception = null;
            try
            {
                Cell.Loop<int>()
                    // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
                    .WithoutCaptures(
                        l =>
                        {
                            StreamSink<Inner> s = Stream.CreateSink<Inner>();
                            Cell<Inner> cc = s.Hold(new Inner(l.SampleLazy()));
                            Cell<int> c = cc.Map(o => o.C).SwitchC();
                            return c;
                        });
            }
            catch (InvalidOperationException e)
            {
                exception = e;
            }
            
            Assert.IsNotNull(exception);
            Assert.AreEqual("ValueFactory attempted to access the Value property of this instance.", exception.Message);
        }

        [Test]
        public void TestLoopAndSwitchC()
        {
            (Cell<int> resultCell, (Cell<Inner> innerCell, StreamSink<Inner> innerStreamSink)) =
                Cell.Loop<int>()
                    .WithCaptures(
                        l =>
                        {
                            StreamSink<Inner> s = Stream.CreateSink<Inner>();
                            Cell<Inner> cc = s.Hold(new Inner(l.SampleLazy()));
                            Cell<int> c = cc.Map(o => o.C).SwitchC().Values().Hold(3);
                            return (Cell: c, Captures: (cc, s));
                        });

            List<int> @out = new List<int>();
            using (resultCell.Listen(@out.Add))
            {
                innerCell.Sample().S.Send(5);
                innerStreamSink.Send(new Inner(resultCell.SampleLazy().Map(v => v - 1)));
                innerCell.Sample().S.Send(7);
            }

            CollectionAssert.AreEqual(new[] { 3, 5, 4, 7 }, @out);
        }

        private class Inner
        {
            public Inner(Lazy<int> initialValue)
            {
                this.S = Stream.CreateSink<int>();
                this.C = this.S.HoldLazy(initialValue);
            }

            public StreamSink<int> S { get; }
            public Cell<int> C { get; }
        }
    }
}