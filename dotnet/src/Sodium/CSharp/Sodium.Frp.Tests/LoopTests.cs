using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using NUnit.Framework;

namespace Sodium.Frp.Tests
{
    [TestFixture]
    public class LoopTests
    {
        #region StreamLoop

        [Test]
        public void ImperativeStreamLoop()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            Stream<int> result = Transaction.Run(
                () =>
                {
                    StreamLoop<int> l = new StreamLoop<int>();
                    Stream<int> resultLocal = s.Snapshot(l.Hold(0), (n, o) => n + o);
                    l.Loop(resultLocal);
                    return resultLocal;
                });

            List<int> @out = new List<int>();
            using (result.Listen(@out.Add))
            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
            }

            CollectionAssert.AreEqual(new[] { 1, 3, 6 }, @out);
        }

        [Test]
        public void ImperativeStreamLoopFailsWhenLoopedTwice()
        {
            InvalidOperationException actual = null;

            try
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                Transaction.RunVoid(
                    () =>
                    {
                        StreamLoop<int> l = new StreamLoop<int>();
                        Stream<int> resultLocal = s.Snapshot(l.Hold(0), (n, o) => n + o);
                        l.Loop(resultLocal);
                        l.Loop(resultLocal);
                    });
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop was looped more than once.", actual.Message);
        }

        [Test]
        public void ImperativeStreamLoopFailsWithoutTransaction()
        {
            InvalidOperationException actual = null;

            try
            {
                // ReSharper disable once ObjectCreationAsStatement
                new StreamLoop<int>();
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop must be created within an explicit transaction.", actual.Message);
        }

        [Test]
        public void ImperativeStreamLoopFailsWhenNotLooped()
        {
            InvalidOperationException actual = null;

            try
            {
                // ReSharper disable once ObjectCreationAsStatement
                Transaction.RunVoid(() => new StreamLoop<int>());
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop was not looped.", actual.Message);
        }

        [Test]
        public void ImperativeStreamLoopFailsWhenLoopedInSeparateTransaction()
        {
            InvalidOperationException actual = null;

            StreamLoop<int> l = null;

            new Thread(
                () =>
                    Transaction.RunVoid(
                        () =>
                        {
                            l = new StreamLoop<int>();
                            Thread.Sleep(500);
                        })).Start();

            try
            {
                StreamSink<int> s = Stream.CreateSink<int>();
                Transaction.RunVoid(
                    () =>
                    {
                        Thread.Sleep(250);
                        Stream<int> resultLocal = s.Snapshot(l.Hold(0), (n, o) => n + o);
                        l.Loop(resultLocal);
                    });
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Thread.Sleep(500);

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop must be looped in the same transaction that it was created in.", actual.Message);
        }

        [Test]
        public void FunctionalStreamLoop()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            Stream<int> result = Stream.Loop<int>().WithoutCaptures(l => s.Snapshot(l.Hold(0), (n, o) => n + o));

            List<int> @out = new List<int>();
            using (result.Listen(@out.Add))
            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
            }

            CollectionAssert.AreEqual(new[] { 1, 3, 6 }, @out);
        }

        [Test]
        public void FunctionalStreamLoopWithCaptures()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            (Stream<int> result, Stream<int> s2) = Stream.Loop<int>()
                .WithCaptures(l => (Stream: s.Snapshot(l.Hold(0), (n, o) => n + o), Captures: s.Map(v => 2 * v)));

            List<int> @out = new List<int>();
            List<int> out2 = new List<int>();
            using (result.Listen(@out.Add))
            using (s2.Listen(out2.Add))

            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
            }

            CollectionAssert.AreEqual(new[] { 1, 3, 6 }, @out);
            CollectionAssert.AreEqual(new[] { 2, 4, 6 }, out2);
        }

        #endregion StreamLoop

        #region BehaviorLoop

        [Test]
        public void ImperativeBehaviorLoop()
        {
            BehaviorSink<int> s = Behavior.CreateSink(0);
            Behavior<int> result = Transaction.Run(
                () =>
                {
                    BehaviorLoop<int> l = new BehaviorLoop<int>();
                    Behavior<int> resultLocal = Operational.Updates(s).Snapshot(l, (n, o) => n + o).Hold(0).AsBehavior();
                    l.Loop(resultLocal);
                    return resultLocal;
                });

            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(result).Listen(@out.Add)))
            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
            }

            CollectionAssert.AreEqual(new[] { 0, 1, 3, 6 }, @out);
        }

        [Test]
        public void ImperativeBehaviorLoopFailsWhenLoopedTwice()
        {
            InvalidOperationException actual = null;

            try
            {
                BehaviorSink<int> s = Behavior.CreateSink(0);
                Transaction.RunVoid(
                    () =>
                    {
                        BehaviorLoop<int> l = new BehaviorLoop<int>();
                        Behavior<int> resultLocal = Operational.Updates(s).Snapshot(l, (n, o) => n + o).Hold(0).AsBehavior();
                        l.Loop(resultLocal);
                        l.Loop(resultLocal);
                    });
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop was looped more than once.", actual.Message);
        }

        [Test]
        public void ImperativeBehaviorLoopFailsWithoutTransaction()
        {
            InvalidOperationException actual = null;

            try
            {
                // ReSharper disable once ObjectCreationAsStatement
                new BehaviorLoop<int>();
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop must be created within an explicit transaction.", actual.Message);
        }

        [Test]
        public void ImperativeBehaviorLoopFailsWhenNotLooped()
        {
            InvalidOperationException actual = null;

            try
            {
                // ReSharper disable once ObjectCreationAsStatement
                Transaction.RunVoid(() => new BehaviorLoop<int>());
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop was not looped.", actual.Message);
        }

        [Test]
        public void ImperativeBehaviorLoopFailsWhenLoopedInSeparateTransaction()
        {
            InvalidOperationException actual = null;

            BehaviorLoop<int> l = null;

            new Thread(
                () =>
                    Transaction.RunVoid(
                        () =>
                        {
                            l = new BehaviorLoop<int>();
                            Thread.Sleep(500);
                        })).Start();

            try
            {
                BehaviorSink<int> s = Behavior.CreateSink(0);
                Transaction.RunVoid(
                    () =>
                    {
                        Thread.Sleep(250);
                        Behavior<int> resultLocal = Operational.Updates(s).Snapshot(l, (n, o) => n + o).Hold(0).AsBehavior();
                        l.Loop(resultLocal);
                    });
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Thread.Sleep(500);

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop must be looped in the same transaction that it was created in.", actual.Message);
        }

        [Test]
        public void FunctionalBehaviorLoop()
        {
            BehaviorSink<int> s = Behavior.CreateSink(0);
            Behavior<int> result = Behavior.Loop<int>().WithoutCaptures(l => Operational.Updates(s).Snapshot(l, (n, o) => n + o).Hold(0).AsBehavior());

            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(result).Listen(@out.Add)))
            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
            }

            CollectionAssert.AreEqual(new[] { 0, 1, 3, 6 }, @out);
        }

        [Test]
        public void FunctionalBehaviorLoopWithCaptures()
        {
            BehaviorSink<int> s = Behavior.CreateSink(0);
            (Behavior<int> result, Behavior<int> s2) = Behavior.Loop<int>()
                .WithCaptures(l => (Behavior: Operational.Updates(s).Snapshot(l, (n, o) => n + o).Hold(0).AsBehavior(), Captures: s.Map(v => 2 * v)));

            List<int> @out = new List<int>();
            List<int> out2 = new List<int>();
            using (Transaction.Run(() => Operational.Value(result).Listen(@out.Add)))
            using (Transaction.Run(() => Operational.Value(s2).Listen(out2.Add)))

            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
            }

            CollectionAssert.AreEqual(new[] { 0, 1, 3, 6 }, @out);
            CollectionAssert.AreEqual(new[] { 0, 2, 4, 6 }, out2);
        }

        #endregion BehaviorLoop

        #region CellLoop

        [Test]
        public void ImperativeCellLoop()
        {
            CellSink<int> s = Cell.CreateSink(0);
            Cell<int> result = Transaction.Run(
                () =>
                {
                    CellLoop<int> l = new CellLoop<int>();
                    Cell<int> resultLocal = s.Updates().Snapshot(l, (n, o) => n + o).Hold(0);
                    l.Loop(resultLocal);
                    return resultLocal;
                });

            List<int> @out = new List<int>();
            using (Transaction.Run(() => result.Values().Listen(@out.Add)))
            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
            }

            CollectionAssert.AreEqual(new[] { 0, 1, 3, 6 }, @out);
        }

        [Test]
        public void ImperativeCellLoopFailsWhenLoopedTwice()
        {
            InvalidOperationException actual = null;

            try
            {
                CellSink<int> s = Cell.CreateSink(0);
                Transaction.RunVoid(
                    () =>
                    {
                        CellLoop<int> l = new CellLoop<int>();
                        Cell<int> resultLocal = s.Updates().Snapshot(l, (n, o) => n + o).Hold(0);
                        l.Loop(resultLocal);
                        l.Loop(resultLocal);
                    });
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop was looped more than once.", actual.Message);
        }

        [Test]
        public void ImperativeCellLoopFailsWithoutTransaction()
        {
            InvalidOperationException actual = null;

            try
            {
                // ReSharper disable once ObjectCreationAsStatement
                new CellLoop<int>();
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop must be created within an explicit transaction.", actual.Message);
        }

        [Test]
        public void ImperativeCellLoopFailsWhenNotLooped()
        {
            InvalidOperationException actual = null;

            try
            {
                // ReSharper disable once ObjectCreationAsStatement
                Transaction.RunVoid(() => new CellLoop<int>());
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop was not looped.", actual.Message);
        }

        [Test]
        public void ImperativeCellLoopFailsWhenLoopedInSeparateTransaction()
        {
            InvalidOperationException actual = null;

            CellLoop<int> l = null;

            new Thread(
                () =>
                    Transaction.RunVoid(
                        () =>
                        {
                            l = new CellLoop<int>();
                            Thread.Sleep(500);
                        })).Start();

            try
            {
                CellSink<int> s = Cell.CreateSink(0);
                Transaction.RunVoid(
                    () =>
                    {
                        Thread.Sleep(250);
                        Cell<int> resultLocal = s.Updates().Snapshot(l, (n, o) => n + o).Hold(0);
                        l.Loop(resultLocal);
                    });
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Thread.Sleep(500);

            Assert.IsNotNull(actual);
            Assert.AreEqual("Loop must be looped in the same transaction that it was created in.", actual.Message);
        }

        [Test]
        public void FunctionalCellLoop()
        {
            CellSink<int> s = Cell.CreateSink(0);
            Cell<int> result = Cell.Loop<int>().WithoutCaptures(l => s.Updates().Snapshot(l, (n, o) => n + o).Hold(0));

            List<int> @out = new List<int>();
            using (Transaction.Run(() => result.Listen(@out.Add)))
            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
            }

            CollectionAssert.AreEqual(new[] { 0, 1, 3, 6 }, @out);
        }

        [Test]
        public void FunctionalCellLoopWithCaptures()
        {
            CellSink<int> s = Cell.CreateSink(0);
            (Cell<int> result, Cell<int> s2) = Cell.Loop<int>()
                .WithCaptures(l => (Cell: s.Updates().Snapshot(l, (n, o) => n + o).Hold(0), Captures: s.Map(v => 2 * v)));

            List<int> @out = new List<int>();
            List<int> out2 = new List<int>();
            using (Transaction.Run(() => result.Listen(@out.Add)))
            using (Transaction.Run(() => s2.Listen(out2.Add)))

            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
            }

            CollectionAssert.AreEqual(new[] { 0, 1, 3, 6 }, @out);
            CollectionAssert.AreEqual(new[] { 0, 2, 4, 6 }, out2);
        }

        #endregion CellLoop

        /*
         * Desired behavior:
         *     A list of items of type TestObject are held in a cell.  TestObject contains a cell of type int named Output, which is calculated from other values.
         *     Any time a new TestObject is created, it will have the values for the cells from which Output is calculated.  The sum of all Output values in the list should always be 50 or greater.
         */
        private class DependencyCycleTest
        {
            private class TestObject
            {
                public TestObject()
                {
                    this.Input1 = Cell.CreateStreamSink<int>();
                    Cell<int> input1Cell = this.Input1.Hold(3);

                    this.Input2 = Cell.CreateStreamSink<int>();
                    Cell<int> input2Cell = this.Input2.Hold(2);

                    this.Output = input1Cell.Lift(input2Cell, (i1, i2) => i1 + i2);
                }

                public StreamSink<int> Input1 { get; }
                public StreamSink<int> Input2 { get; }
                public Cell<int> Output { get; }
            }

            /*
             * Switch over the sum of the Output cells in the list.
             * This won't work because we would need to recurse to keep the list correct when the sum is very low (only one item can be added per transaction).
             * The current implementation throws an exception stating that a dependency cycle was detected, and I think this is the correct behavior.
             */
            [Test]
            public void TestSwitchCLoop()
            {
                Exception actual = null;
                try
                {
                    CellStreamSink<IReadOnlyList<TestObject>> streamSink = Cell.CreateStreamSink<IReadOnlyList<TestObject>>();
                    Cell<IReadOnlyList<TestObject>> cell = Transaction.Run(() =>
                    {
                        CellLoop<IReadOnlyList<TestObject>> cellLoop = new CellLoop<IReadOnlyList<TestObject>>();
                        Cell<IReadOnlyList<TestObject>> cellLocal = streamSink.Map(v => (Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>)(_ => v))
                            .Merge(cellLoop.Map(oo => oo.Select(o => o.Output).Lift(vv => vv.Sum())).SwitchC().Updates().Filter(sum => sum < 50).MapTo((Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>)(v => v.Concat(new[] { new TestObject() }).ToArray())), (f, g) => v => g(f(v)))
                            .Snapshot(cellLoop, (f, v) => f(v))
                            .Hold(Enumerable.Range(1, 10).Select(_ => new TestObject()).ToArray());
                        cellLoop.Loop(cellLocal);
                        return cellLocal;
                    });

                    List<int> objectCounts = new List<int>();
                    objectCounts.Add(-1);
                    cell.Listen(vv => objectCounts.Add(vv.Count));
                    objectCounts.Add(-1);
                    cell.Sample()[2].Input1.Send(1);
                    objectCounts.Add(-1);
                    cell.Sample()[1].Input1.Send(-20);
                    objectCounts.Add(-1);
                    streamSink.Send(new TestObject[0]);
                    objectCounts.Add(-1);
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

            /*
             * Switch over the sum of the Output cell value streams in the list.
             * This won't work both because we miss the first Values stream event when the list changes and also because we would need to recurse to keep the list correct when the sum is very low (only one item can be added per transaction).
             */
            [Test]
            public void TestSwitchSValuesLoop()
            {
                CellStreamSink<IReadOnlyList<TestObject>> streamSink = Cell.CreateStreamSink<IReadOnlyList<TestObject>>();
                Cell<IReadOnlyList<TestObject>> cell = Transaction.Run(() =>
                {
                    CellLoop<IReadOnlyList<TestObject>> cellLoop = new CellLoop<IReadOnlyList<TestObject>>();
                    Cell<IReadOnlyList<TestObject>> cellLocal = streamSink.Map(v => (Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>)(_ => v))
                        .Merge(cellLoop.Map(oo => oo.Select(o => o.Output).Lift(vv => vv.Sum()).Values()).SwitchS().Filter(sum => sum < 50).MapTo((Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>)(v => v.Concat(new[] { new TestObject() }).ToArray())), (f, g) => v => g(f(v)))
                        .Snapshot(cellLoop, (f, v) => f(v))
                        .Hold(Enumerable.Range(1, 10).Select(_ => new TestObject()).ToArray());
                    cellLoop.Loop(cellLocal);
                    return cellLocal;
                });

                List<int> objectCounts = new List<int>();
                objectCounts.Add(-1);
                cell.Listen(vv => objectCounts.Add(vv.Count));
                objectCounts.Add(-1);
                cell.Sample()[2].Input1.Send(1);
                objectCounts.Add(-1);
                cell.Sample()[1].Input1.Send(-20);
                objectCounts.Add(-1);
                streamSink.Send(new TestObject[0]);
                objectCounts.Add(-1);

                // Ideal result, likely not achievable.
                //CollectionAssert.AreEquivalent(new[] { -1, 10, -1, 11, -1, 15, -1, 10, -1 }, objectCounts);

                // Glitchy result, also not returned by this method.
                //CollectionAssert.AreEquivalent(new[] { -1, 10, -1, 11, -1, 12, 13, 14, 15, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -1 }, objectCounts);

                // Incorrect result we will see.
                CollectionAssert.AreEquivalent(new[] { -1, 10, -1, 11, -1, 12, -1, 0, -1 }, objectCounts);
            }

            /*
             * Switch over the sum of the Output cells in the list, deferring the firings from the Values stream.
             * This will work because it allows the Values to recurse by firing each step in a new transaction immediately following the transaction for the previous step.
             * The only drawback to this method is that each step of the recursion is in a new transaction, so it exhibits "glitchy" behavior where the intermediate invalid states are externally visible.
             */
            [Test]
            public void TestSwitchCDeferredLoop()
            {
                CellStreamSink<IReadOnlyList<TestObject>> streamSink = Cell.CreateStreamSink<IReadOnlyList<TestObject>>();
                Cell<IReadOnlyList<TestObject>> cell = Transaction.Run(() =>
                {
                    CellLoop<IReadOnlyList<TestObject>> cellLoop = new CellLoop<IReadOnlyList<TestObject>>();
                    Cell<IReadOnlyList<TestObject>> cellLocal = streamSink
                        .OrElse(Operational.Defer(cellLoop.Map(oo => oo.Select(o => o.Output).Lift(vv => vv.Sum())).SwitchC().Values()).Filter(sum => sum < 50).Snapshot(cellLoop, (_, items) => (IReadOnlyList<TestObject>)items.Concat(new[] { new TestObject() }).ToArray()))
                        .Hold(Enumerable.Range(1, 10).Select(_ => new TestObject()).ToArray());
                    cellLoop.Loop(cellLocal);
                    return cellLocal;
                });

                List<int> objectCounts = new List<int>();
                objectCounts.Add(-1);
                cell.Listen(vv => objectCounts.Add(vv.Count));
                objectCounts.Add(-1);
                cell.Sample()[2].Input1.Send(1);
                objectCounts.Add(-1);
                cell.Sample()[1].Input1.Send(-20);
                objectCounts.Add(-1);
                streamSink.Send(new TestObject[0]);
                objectCounts.Add(-1);

                // Ideal result, likely not achievable.
                //CollectionAssert.AreEquivalent(new[] { -1, 10, -1, 11, -1, 15, -1, 10, -1 }, objectCounts);

                // Glitchy result, but correct otherwise.
                CollectionAssert.AreEquivalent(new[] { -1, 10, -1, 11, -1, 12, 13, 14, 15, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -1 }, objectCounts);
            }

            /*
             * Switch over the sum of the Output cells in the list, deferring the firings from the Values stream, and use a better API.
             * This is identical to the previous solution except that it uses a special version of SwitchC which defers the Values stream firings.
             * Using this API, we can better capture the intent of the SwitchC call and allow the type system to eventually check for valid usages of the looped cell.
             * Note that when the types are modified, the SwitchCWithDeferredValues() call will actually become SwitchC().DeferredValues() with SwitchC() on the cell loop returning a special type
             * containing the DeferredValues() and DeferredUpdates() methods.
             */
            [Test]
            public void TestSwitchCDeferredLoopWithBetterApi()
            {
                CellStreamSink<IReadOnlyList<TestObject>> streamSink = Cell.CreateStreamSink<IReadOnlyList<TestObject>>();
                Cell<IReadOnlyList<TestObject>> cell = Transaction.Run(() =>
                {
                    CellLoop<IReadOnlyList<TestObject>> cellLoop = new CellLoop<IReadOnlyList<TestObject>>();
                    Cell<IReadOnlyList<TestObject>> cellLocal = streamSink
                        .OrElse(cellLoop.Map(oo => oo.Select(o => o.Output).Lift(vv => vv.Sum())).SwitchCWithDeferredValues().Filter(sum => sum < 50).Snapshot(cellLoop, (_, items) => (IReadOnlyList<TestObject>)items.Concat(new[] { new TestObject() }).ToArray()))
                        .Hold(Enumerable.Range(1, 10).Select(_ => new TestObject()).ToArray());
                    cellLoop.Loop(cellLocal);
                    return cellLocal;
                });

                List<int> objectCounts = new List<int>();
                objectCounts.Add(-1);
                cell.Listen(vv => objectCounts.Add(vv.Count));
                objectCounts.Add(-1);
                cell.Sample()[2].Input1.Send(1);
                objectCounts.Add(-1);
                cell.Sample()[1].Input1.Send(-20);
                objectCounts.Add(-1);
                streamSink.Send(new TestObject[0]);
                objectCounts.Add(-1);

                // Ideal result, likely not achievable.
                //CollectionAssert.AreEquivalent(new[] { -1, 10, -1, 11, -1, 15, -1, 10, -1 }, objectCounts);

                // Glitchy result, but correct otherwise.
                CollectionAssert.AreEquivalent(new[] { -1, 10, -1, 11, -1, 12, 13, 14, 15, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -1 }, objectCounts);
            }
        }
    }

    public static class TestExtensions
    {
        public static Stream<T> SwitchCWithDeferredUpdates<T>(this Cell<Cell<T>> cca)
        {
            return Operational.Defer(cca.SwitchC().Updates());
        }

        public static Stream<T> SwitchCWithDeferredValues<T>(this Cell<Cell<T>> cca)
        {
            return Operational.Defer(cca.SwitchC().Values());
        }
    }
}