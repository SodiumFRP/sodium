using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class StreamTests
    {
        [Test]
        public void TestStreamSend()
        {
            StreamSink<int> s = new StreamSink<int>();
            List<int> @out = new List<int>();
            IListener l = s.Listen(@out.Add);
            s.Send(5);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 5 }, @out);
            s.Send(6);
            CollectionAssert.AreEqual(new[] { 5 }, @out);
        }

        [Test]
        public void TestMap()
        {
            StreamSink<int> s = new StreamSink<int>();
            Stream<string> m = s.Map(x => (x + 2).ToString());
            List<string> @out = new List<string>();
            IListener l = m.Listen(@out.Add);
            s.Send(5);
            s.Send(3);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "7", "5" }, @out);
        }

        [Test]
        public void TestOrElseNonSimultaneous()
        {
            StreamSink<int> s1 = new StreamSink<int>();
            StreamSink<int> s2 = new StreamSink<int>();
            List<int> @out = new List<int>();
            IListener l = s1.OrElse(s2).Listen(@out.Add);
            s1.Send(7);
            s2.Send(9);
            s1.Send(8);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 7, 9, 8 }, @out);
        }

        [Test]
        public void TestOrElseSimultaneous1()
        {
            StreamSink<int> s1 = new StreamSink<int>((_, r) => r);
            StreamSink<int> s2 = new StreamSink<int>((_, r) => r);
            List<int> @out = new List<int>();
            IListener l = s2.OrElse(s1).Listen(@out.Add);
            Transaction.RunVoid(() =>
            {
                s1.Send(7);
                s2.Send(60);
            });
            Transaction.RunVoid(() =>
            {
                s1.Send(9);
            });
            Transaction.RunVoid(() =>
            {
                s1.Send(7);
                s1.Send(60);
                s2.Send(8);
                s2.Send(90);
            });
            Transaction.RunVoid(() =>
            {
                s2.Send(8);
                s2.Send(90);
                s1.Send(7);
                s1.Send(60);
            });
            Transaction.RunVoid(() =>
            {
                s2.Send(8);
                s1.Send(7);
                s2.Send(90);
                s1.Send(60);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 60, 9, 90, 90, 90 }, @out);
        }

        [Test]
        public void TestOrElseSimultaneous2()
        {
            StreamSink<int> s = new StreamSink<int>();
            Stream<int> s2 = s.Map(x => 2 * x);
            List<int> @out = new List<int>();
            IListener l = s.OrElse(s2).Listen(@out.Add);
            s.Send(7);
            s.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 7, 9 }, @out);
        }

        [Test]
        public void TestOrElseLeftBias()
        {
            StreamSink<int> s = new StreamSink<int>();
            Stream<int> s2 = s.Map(x => 2 * x);
            List<int> @out = new List<int>();
            IListener l = s2.OrElse(s).Listen(@out.Add);
            s.Send(7);
            s.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 14, 18 }, @out);
        }

        [Test]
        public void TestMergeNonSimultaneous()
        {
            StreamSink<int> s1 = new StreamSink<int>();
            StreamSink<int> s2 = new StreamSink<int>();
            List<int> @out = new List<int>();
            IListener l = s1.Merge(s2, (x, y) => x + y).Listen(@out.Add);
            s1.Send(7);
            s2.Send(9);
            s1.Send(8);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 7, 9, 8 }, @out);
        }

        [Test]
        public void TestMergeSimultaneous()
        {
            StreamSink<int> s = new StreamSink<int>();
            Stream<int> s2 = s.Map(x => 2 * x);
            List<int> @out = new List<int>();
            IListener l = s.Merge(s2, (x, y) => x + y).Listen(@out.Add);
            s.Send(7);
            s.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 21, 27 }, @out);
        }

        [Test]
        public void TestCoalesce1()
        {
            StreamSink<int> s = new StreamSink<int>((x, y) => x + y);
            List<int> @out = new List<int>();
            IListener l = s.Listen(@out.Add);
            Transaction.RunVoid(() =>
            {
                s.Send(2);
            });
            Transaction.RunVoid(() =>
            {
                s.Send(8);
                s.Send(40);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 48 }, @out.ToArray());
        }

        [Test]
        public void TestCoalesce2()
        {
            StreamSink<int> s = new StreamSink<int>((x, y) => x + y);
            List<int> @out = new List<int>();
            IListener l = s.Listen(@out.Add);
            Transaction.RunVoid(() =>
            {
                s.Send(1);
                s.Send(2);
                s.Send(3);
                s.Send(4);
                s.Send(5);
            });
            Transaction.RunVoid(() =>
            {
                s.Send(6);
                s.Send(7);
                s.Send(8);
                s.Send(9);
                s.Send(10);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 15, 40 }, @out.ToArray());
        }

        [Test]
        public void TestFilter()
        {
            StreamSink<char> s = new StreamSink<char>();
            List<char> @out = new List<char>();
            IListener l = s.Filter(char.IsUpper).Listen(@out.Add);
            s.Send('H');
            s.Send('o');
            s.Send('I');
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 'H', 'I' }, @out);
        }

        [Test]
        public void TestFilterMaybe()
        {
            StreamSink<IMaybe<string>> s = new StreamSink<IMaybe<string>>();
            List<string> @out = new List<string>();
            IListener l = s.FilterMaybe().Listen(@out.Add);
            s.Send(Maybe.Just("tomato"));
            s.Send(Maybe.Nothing<string>());
            s.Send(Maybe.Just("peach"));
            s.Send(Maybe.Just<string>(null));
            s.Send(Maybe.Just("pear"));
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "tomato", "peach", null, "pear" }, @out);
        }

        [Test]
        public void TestLoopStream()
        {
            StreamSink<int> sa = new StreamSink<int>();
            Tuple<StreamLoop<int>, Stream<int>, Stream<int>> s = Transaction.Run(() =>
             {
                 StreamLoop<int> sbLocal = new StreamLoop<int>();
                 Stream<int> scLocal = sa.Map(x => x % 10).Merge(sbLocal, (x, y) => x * y);
                 Stream<int> sbOut = sa.Map(x => x / 10).Filter(x => x != 0);
                 sbLocal.Loop(sbOut);
                 return Tuple.Create(sbLocal, sbOut, scLocal);
             });
            StreamLoop<int> sb = s.Item1;
            Stream<int> sb2 = s.Item2;
            Stream<int> sc = s.Item3;
            List<int> @out = new List<int>();
            List<int> out2 = new List<int>();
            List<int> out3 = new List<int>();
            IListener l = sb.Listen(@out.Add);
            IListener l2 = sb2.Listen(out2.Add);
            IListener l3 = sc.Listen(out3.Add);
            sa.Send(2);
            sa.Send(52);
            l3.Unlisten();
            l2.Unlisten();
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 5 }, @out.ToArray());
            CollectionAssert.AreEqual(new[] { 5 }, out2.ToArray());
            CollectionAssert.AreEqual(new[] { 2, 10 }, out3.ToArray());
        }

        [Test]
        public void TestLoopCell()
        {
            DiscreteCellSink<int> ca = DiscreteCell.CreateSink(22);
            Tuple<DiscreteCellLoop<int>, DiscreteCell<int>, DiscreteCell<int>> c = Transaction.Run(() =>
            {
                DiscreteCellLoop<int> cbLocal = DiscreteCell.CreateLoop<int>();
                DiscreteCell<int> ccLocal = ca.Map(x => x % 10).Lift(cbLocal, (x, y) => x * y);
                DiscreteCell<int> cbOut = ca.Map(x => x / 10);
                cbLocal.Loop(cbOut);
                return Tuple.Create(cbLocal, cbOut, ccLocal);
            });
            DiscreteCellLoop<int> cb = c.Item1;
            DiscreteCell<int> cb2 = c.Item2;
            DiscreteCell<int> cc = c.Item3;
            List<int> @out = new List<int>();
            List<int> out2 = new List<int>();
            List<int> out3 = new List<int>();
            IListener l = cb.Listen(@out.Add);
            IListener l2 = cb2.Listen(out2.Add);
            IListener l3 = cc.Listen(out3.Add);
            ca.Send(2);
            ca.Send(52);
            l3.Unlisten();
            l2.Unlisten();
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 0, 5 }, @out.ToArray());
            CollectionAssert.AreEqual(new[] { 2, 0, 5 }, out2.ToArray());
            CollectionAssert.AreEqual(new[] { 4, 0, 10 }, out3.ToArray());
        }

        [Test]
        public void TestGate()
        {
            StreamSink<char?> sc = new StreamSink<char?>();
            CellSink<bool> cGate = new CellSink<bool>(true);
            List<char?> @out = new List<char?>();
            IListener l = sc.Gate(cGate).Listen(@out.Add);
            sc.Send('H');
            cGate.Send(false);
            sc.Send('O');
            cGate.Send(true);
            sc.Send('I');
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 'H', 'I' }, @out);
        }

        [Test]
        public void TestCalm()
        {
            StreamSink<int> s = new StreamSink<int>();
            List<int> @out = new List<int>();
            IListener l = s.Calm().Listen(@out.Add);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(4);
            s.Send(2);
            s.Send(4);
            s.Send(4);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(4);
            s.Send(2);
            s.Send(4);
            s.Send(4);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(4);
            s.Send(2);
            s.Send(4);
            s.Send(4);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(4);
            s.Send(2);
            s.Send(4);
            s.Send(4);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(2);
            s.Send(4);
            s.Send(2);
            s.Send(4);
            s.Send(4);
            s.Send(2);
            s.Send(2);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2 }, @out);
        }

        [Test]
        public void TestCalm2()
        {
            StreamSink<int> s = new StreamSink<int>();
            List<int> @out = new List<int>();
            IListener l = s.Calm().Listen(@out.Add);
            s.Send(2);
            s.Send(4);
            s.Send(2);
            s.Send(4);
            s.Send(4);
            s.Send(2);
            s.Send(2);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 2, 4, 2, 4, 2 }, @out);
        }

        [Test]
        public void TestCollect()
        {
            StreamSink<int> sa = new StreamSink<int>();
            List<int> @out = new List<int>();
            Stream<int> sum = sa.Collect(Tuple.Create(100, true), (a, s) =>
            {
                int outputValue = s.Item1 + (s.Item2 ? a * 3 : a);
                return Tuple.Create(outputValue, Tuple.Create(outputValue, outputValue % 2 == 0));
            });
            IListener l = sum.Listen(@out.Add);
            sa.Send(5);
            sa.Send(7);
            sa.Send(1);
            sa.Send(2);
            sa.Send(3);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 115, 122, 125, 127, 130 }, @out);
        }

        [Test]
        public void TestAccum()
        {
            StreamSink<int> sa = new StreamSink<int>();
            List<int> @out = new List<int>();
            DiscreteCell<int> sum = sa.Accum(100, (a, s) => a + s);
            IListener l = sum.Listen(@out.Add);
            sa.Send(5);
            sa.Send(7);
            sa.Send(1);
            sa.Send(2);
            sa.Send(3);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 100, 105, 112, 113, 115, 118 }, @out);
        }

        [Test]
        public void TestOnce()
        {
            StreamSink<char> s = new StreamSink<char>();
            List<char> @out = new List<char>();
            IListener l = s.Once().Listen(@out.Add);
            s.Send('A');
            s.Send('B');
            s.Send('C');
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 'A' }, @out);
        }

        [Test]
        public void TestHold()
        {
            StreamSink<char> s = new StreamSink<char>();
            DiscreteCell<char> c = s.Hold(' ');
            List<char> @out = new List<char>();
            IListener l = c.Listen(@out.Add);
            s.Send('C');
            s.Send('B');
            s.Send('A');
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { ' ', 'C', 'B', 'A' }, @out);
        }

        [Test]
        public void TestHoldImplicitDelay()
        {
            StreamSink<char> s = new StreamSink<char>();
            DiscreteCell<char> c = s.Hold(' ');
            List<char> @out = new List<char>();
            IListener l = s.Snapshot(c.Cell).Listen(@out.Add);
            s.Send('C');
            s.Send('B');
            s.Send('A');
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { ' ', 'C', 'B' }, @out);
        }

        [Test]
        public void TestDefer()
        {
            StreamSink<char> s = new StreamSink<char>();
            DiscreteCell<char> c = s.Hold(' ');
            List<char> @out = new List<char>();
            IListener l = Operational.Defer(s).Snapshot(c.Cell).Listen(@out.Add);
            s.Send('C');
            s.Send('B');
            s.Send('A');
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 'C', 'B', 'A' }, @out);
        }

        [Test]
        public void TestListen()
        {
            StreamSink<int> s = new StreamSink<int>();

            List<int> @out = new List<int>();

            ((Action)(() =>
            {
                // ReSharper disable once UnusedVariable
                IListener l = s.Listen(@out.Add);

                s.Send(1);
                s.Send(2);
            }))();

            GC.Collect(0, GCCollectionMode.Forced);
            s.Send(3);
            s.Send(4);

            Assert.AreEqual(2, @out.Count);
        }

        [Test]
        public void TestListenWithMap()
        {
            StreamSink<int> s = new StreamSink<int>();

            List<int> @out = new List<int>();

            ((Action)(() =>
            {
                Stream<int> s2 = s.Map(v => v + 1);

                ((Action)(() =>
                {
                    // ReSharper disable once UnusedVariable
                    s2.Listen(@out.Add);

                    s.Send(1);
                    s.Send(2);
                }))();

                GC.Collect(0, GCCollectionMode.Forced);

                ((Action)(() =>
                {
                    // ReSharper disable once UnusedVariable
                    s2.Listen(@out.Add);

                    s.Send(3);
                    s.Send(4);
                    s.Send(5);
                }))();
            }))();

            GC.Collect(0, GCCollectionMode.Forced);
            s.Send(6);
            s.Send(7);

            Assert.AreEqual(5, @out.Count);
        }

        [Test]
        public void TestUnlisten()
        {
            StreamSink<int> s = new StreamSink<int>();

            List<int> @out = new List<int>();

            ((Action)(() =>
            {
                // ReSharper disable once UnusedVariable
                IListener l = s.Listen(@out.Add);

                s.Send(1);

                l.Unlisten();

                s.Send(2);
            }))();

            GC.Collect(0, GCCollectionMode.Forced);
            s.Send(3);
            s.Send(4);

            Assert.AreEqual(1, @out.Count);
        }

        [Test]
        public void TestMultipleUnlisten()
        {
            StreamSink<int> s = new StreamSink<int>();

            List<int> @out = new List<int>();

            ((Action)(() =>
            {
                // ReSharper disable once UnusedVariable
                IListener l = s.Listen(@out.Add);

                s.Send(1);

                l.Unlisten();
                l.Unlisten();

                s.Send(2);

                l.Unlisten();
            }))();

            GC.Collect(0, GCCollectionMode.Forced);
            s.Send(3);
            s.Send(4);

            Assert.AreEqual(1, @out.Count);
        }

        [Test]
        public void TestListenOnce()
        {
            StreamSink<char> s = new StreamSink<char>();
            List<char> @out = new List<char>();
            IListener l = s.ListenOnce(@out.Add);
            s.Send('A');
            s.Send('B');
            s.Send('C');
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 'A' }, @out);
        }

        [Test]
        public async Task TestListenOnceTask()
        {
            StreamSink<char> s = new StreamSink<char>();
            new Thread(() =>
            {
                Thread.Sleep(250);
                s.Send('A');
                s.Send('B');
                s.Send('C');
            }).Start();
            TaskWithListener<char> t = s.ListenOnce();
            GC.Collect(0, GCCollectionMode.Forced);
            char r = await t;
            Assert.AreEqual('A', r);
        }

        [Test]
        public async Task TestListenAsync()
        {
            DiscreteCellSink<int> a = DiscreteCell.CreateSink(1);
            DiscreteCell<int> a1 = a.Map(x => x + 1);
            DiscreteCell<int> a2 = a.Map(x => x * 2);
            Tuple<List<int>, DiscreteCellLoop<int>> resultsAndCalled = Transaction.Run(() =>
            {
                DiscreteCell<int> result = a1.Lift(a2, (x, y) => x + y);
                Stream<Unit> incrementStream = Operational.Value(result.Cell).MapTo(Unit.Value);
                StreamSink<Unit> decrementStream = new StreamSink<Unit>();
                DiscreteCellLoop<int> calledLoop = DiscreteCell.CreateLoop<int>();
                calledLoop.Loop(incrementStream.MapTo(1).Merge(decrementStream.MapTo(-1), (x, y) => x + y).Snapshot(calledLoop.Cell, (u, c) => c + u).Hold(0));
                List<int> r = new List<int>();
                result.Listen(v =>
                {
                    Task.Run(async () =>
                    {
                        await Task.Delay(900);
                        r.Add(v);
                        decrementStream.Send(Unit.Value);
                    });
                });
                return Tuple.Create(r, calledLoop);
            });
            // ReSharper disable once UnusedVariable
            List<int> results = resultsAndCalled.Item1;
            DiscreteCell<int> called = resultsAndCalled.Item2;
            List<int> calledResults = new List<int>();
            called.Listen(calledResults.Add);

            await Task.Delay(500);
            a.Send(2);
            await Task.Delay(500);
            a.Send(3);
            await Task.Delay(2500);
        }
    }
}