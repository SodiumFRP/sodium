using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class LoopTests
    {
        /*
         * Desired behavior:
         *     A list of items of type TestObject are held in a Cell.  TestObject contains a Cell of type int named Output, which is calculated from other values.
         *     Any time a new TestObject is created, it will have the values for the cells from which Output is calculated.  The sum of all Output values in the list should always be 50 or greater.
         */
        private class DependencyCycleTest
        {
            private class TestObject
            {
                public TestObject()
                {
                    this.Input1 = new DiscreteCellStreamSink<int>();
                    DiscreteCell<int> input1Cell = this.Input1.Hold(3);

                    this.Input2 = new DiscreteCellStreamSink<int>();
                    DiscreteCell<int> input2Cell = this.Input2.Hold(2);

                    this.Output = input1Cell.Lift(input2Cell, (i1, i2) => i1 + i2);
                }

                public StreamSink<int> Input1 { get; }
                public StreamSink<int> Input2 { get; }
                public DiscreteCell<int> Output { get; }
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
                    DiscreteCellStreamSink<IReadOnlyList<TestObject>> streamSink = new DiscreteCellStreamSink<IReadOnlyList<TestObject>>();
                    DiscreteCell<IReadOnlyList<TestObject>> cell = Transaction.Run(() =>
                    {
                        DiscreteCellLoop<IReadOnlyList<TestObject>> cellLoop = new DiscreteCellLoop<IReadOnlyList<TestObject>>();
                        DiscreteCell<IReadOnlyList<TestObject>> cellLocal = streamSink.Map<Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>>(v => _ => v)
                            .Merge(cellLoop.Map(oo => oo.Select(o => o.Output).Lift(vv => vv.Sum())).SwitchC().Updates.Filter(sum => sum < 50).MapTo<Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>>(v => v.Concat(new[] { new TestObject() }).ToArray()), (f, g) => v => g(f(v)))
                            .Snapshot(cellLoop, (f, v) => f(v))
                            .Hold(Enumerable.Range(1, 10).Select(_ => new TestObject()).ToArray());
                        cellLoop.Loop(cellLocal);
                        return cellLocal;
                    });

                    List<int> objectCounts = new List<int>();
                    objectCounts.Add(-1);
                    cell.Listen(vv => objectCounts.Add(vv.Count));
                    objectCounts.Add(-1);
                    cell.Cell.Sample()[2].Input1.Send(1);
                    objectCounts.Add(-1);
                    cell.Cell.Sample()[1].Input1.Send(-20);
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
             * This won't work both because we miss the first Values stream event when the list changes and also because would need to recurse to keep the list correct when the sum is very low (only one item can be added per transaction).
             * The current implementation throws an exception stating that a dependency cycle was detected, and I think this is the correct behavior.
             */
            [Test]
            public void TestSwitchSValuesLoop()
            {
                DiscreteCellStreamSink<IReadOnlyList<TestObject>> streamSink = new DiscreteCellStreamSink<IReadOnlyList<TestObject>>();
                DiscreteCell<IReadOnlyList<TestObject>> cell = Transaction.Run(() =>
                {
                    DiscreteCellLoop<IReadOnlyList<TestObject>> cellLoop = new DiscreteCellLoop<IReadOnlyList<TestObject>>();
                    DiscreteCell<IReadOnlyList<TestObject>> cellLocal = streamSink.Map<Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>>(v => _ => v)
                        .Merge(cellLoop.Map(oo => oo.Select(o => o.Output).Lift(vv => vv.Sum()).Values).SwitchS().Filter(sum => sum < 50).MapTo<Func<IReadOnlyList<TestObject>, IReadOnlyList<TestObject>>>(v => v.Concat(new[] { new TestObject() }).ToArray()), (f, g) => v => g(f(v)))
                        .Snapshot(cellLoop, (f, v) => f(v))
                        .Hold(Enumerable.Range(1, 10).Select(_ => new TestObject()).ToArray());
                    cellLoop.Loop(cellLocal);
                    return cellLocal;
                });

                List<int> objectCounts = new List<int>();
                objectCounts.Add(-1);
                cell.Listen(vv => objectCounts.Add(vv.Count));
                objectCounts.Add(-1);
                cell.Cell.Sample()[2].Input1.Send(1);
                objectCounts.Add(-1);
                cell.Cell.Sample()[1].Input1.Send(-20);
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
                DiscreteCellStreamSink<IReadOnlyList<TestObject>> streamSink = new DiscreteCellStreamSink<IReadOnlyList<TestObject>>();
                DiscreteCell<IReadOnlyList<TestObject>> cell = Transaction.Run(() =>
                {
                    DiscreteCellLoop<IReadOnlyList<TestObject>> cellLoop = new DiscreteCellLoop<IReadOnlyList<TestObject>>();
                    DiscreteCell<IReadOnlyList<TestObject>> cellLocal = streamSink
                        .OrElse(Operational.Defer(cellLoop.Map(oo => oo.Select(o => o.Output).Lift(vv => vv.Sum())).SwitchC().Values).Filter(sum => sum < 50).Snapshot(cellLoop, (_, items) => (IReadOnlyList<TestObject>)items.Concat(new[] { new TestObject() }).ToArray()))
                        .Hold(Enumerable.Range(1, 10).Select(_ => new TestObject()).ToArray());
                    cellLoop.Loop(cellLocal);
                    return cellLocal;
                });

                List<int> objectCounts = new List<int>();
                objectCounts.Add(-1);
                cell.Listen(vv => objectCounts.Add(vv.Count));
                objectCounts.Add(-1);
                cell.Cell.Sample()[2].Input1.Send(1);
                objectCounts.Add(-1);
                cell.Cell.Sample()[1].Input1.Send(-20);
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
                DiscreteCellStreamSink<IReadOnlyList<TestObject>> streamSink = new DiscreteCellStreamSink<IReadOnlyList<TestObject>>();
                DiscreteCell<IReadOnlyList<TestObject>> cell = Transaction.Run(() =>
                {
                    DiscreteCellLoop<IReadOnlyList<TestObject>> cellLoop = new DiscreteCellLoop<IReadOnlyList<TestObject>>();
                    DiscreteCell<IReadOnlyList<TestObject>> cellLocal = streamSink
                        .OrElse(Operational.Defer(cellLoop.Map(oo => oo.Select(o => o.Output).Lift(vv => vv.Sum())).SwitchCWithDeferredValues()).Filter(sum => sum < 50).Snapshot(cellLoop, (_, items) => (IReadOnlyList<TestObject>)items.Concat(new[] { new TestObject() }).ToArray()))
                        .Hold(Enumerable.Range(1, 10).Select(_ => new TestObject()).ToArray());
                    cellLoop.Loop(cellLocal);
                    return cellLocal;
                });

                List<int> objectCounts = new List<int>();
                objectCounts.Add(-1);
                cell.Listen(vv => objectCounts.Add(vv.Count));
                objectCounts.Add(-1);
                cell.Cell.Sample()[2].Input1.Send(1);
                objectCounts.Add(-1);
                cell.Cell.Sample()[1].Input1.Send(-20);
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
        public static Stream<T> SwitchCWithDeferredUpdates<T>(this DiscreteCell<DiscreteCell<T>> cca)
        {
            return Operational.Defer(cca.SwitchC().Updates);
        }

        public static Stream<T> SwitchCWithDeferredValues<T>(this DiscreteCell<DiscreteCell<T>> cca)
        {
            return Operational.Defer(cca.SwitchC().Values);
        }
    }
}