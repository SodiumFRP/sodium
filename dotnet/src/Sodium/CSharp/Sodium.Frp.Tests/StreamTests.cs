using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests
{
    [TestFixture]
    public class StreamTests
    {
        [Test]
        public void TestStreamSend()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
            List<int> @out = new List<int>();
            IListener l = s.Listen(@out.Add);
            s.Send(5);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 5 }, @out);
            s.Send(6);
            CollectionAssert.AreEqual(new[] { 5 }, @out);
        }

        [Test]
        public void TestStreamSendInCallbackThrowsException()
        {
            InvalidOperationException actual = null;

            StreamSink<int> s = Stream.CreateSink<int>();
            StreamSink<int> s2 = Stream.CreateSink<int>();
            using (s.Listen(s2.Send))
            {
                try
                {
                    s.Send(5);
                }
                catch (InvalidOperationException e)
                {
                    actual = e;
                }
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Send may not be called inside a Sodium callback.", actual.Message);
        }

        [Test]
        public void TestStreamSendInMapThrowsException()
        {
            InvalidOperationException actual = null;

            StreamSink<int> s = Stream.CreateSink<int>();
            StreamSink<int> s2 = Stream.CreateSink<int>();
            using (s.Map(
                    v =>
                    {
                        s2.Send(v);
                        return Unit.Value;
                    })
                .Listen(_ => { }))
            {
                try
                {
                    s.Send(5);
                }
                catch (InvalidOperationException e)
                {
                    actual = e;
                }
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Send may not be called inside a Sodium callback.", actual.Message);
        }

        [Test]
        public void TestStreamSendInCellMapThrowsException()
        {
            InvalidOperationException actual = null;

            CellSink<int> c = Cell.CreateSink(5);
            StreamSink<int> s2 = Stream.CreateSink<int>();
            try
            {
                using (c.Map(
                        v =>
                        {
                            s2.Send(v);
                            return Unit.Value;
                        })
                    .Listen(_ => { }))
                {
                }
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Send may not be called inside a Sodium callback.", actual.Message);
        }

        [Test]
        public void TestStreamSendInCellLiftThrowsException()
        {
            InvalidOperationException actual = null;

            Cell<int> c = Cell.Constant(5);
            Cell<int> c2 = Cell.Constant(7);
            StreamSink<int> s2 = Stream.CreateSink<int>();
            try
            {
                using (c.Lift(
                        c2,
                        (_, __) =>
                        {
                            s2.Send(5);
                            return Unit.Value;
                        })
                    .Listen(_ => { }))
                {
                }
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Send may not be called inside a Sodium callback.", actual.Message);
        }

        [Test]
        public void TestStreamSendInCellApplyThrowsException()
        {
            InvalidOperationException actual = null;

            Cell<int> c = Cell.Constant(5);
            StreamSink<int> s2 = Stream.CreateSink<int>();
            Cell<Func<int, Unit>> c2 = Cell.Constant<Func<int, Unit>>(
                _ =>
                {
                    s2.Send(5);
                    return Unit.Value;
                });
            try
            {
                using (c.Apply(c2).Listen(_ => { }))
                {
                }
            }
            catch (InvalidOperationException e)
            {
                actual = e;
            }

            Assert.IsNotNull(actual);
            Assert.AreEqual("Send may not be called inside a Sodium callback.", actual.Message);
        }

        [Test]
        public void TestMap()
        {
            StreamSink<int> s = Stream.CreateSink<int>();
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
            StreamSink<int> s1 = Stream.CreateSink<int>();
            StreamSink<int> s2 = Stream.CreateSink<int>();
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
            StreamSink<int> s1 = Stream.CreateSink<int>((_, r) => r);
            StreamSink<int> s2 = Stream.CreateSink<int>((_, r) => r);
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
            StreamSink<int> s = Stream.CreateSink<int>();
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
            StreamSink<int> s = Stream.CreateSink<int>();
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
            StreamSink<int> s1 = Stream.CreateSink<int>();
            StreamSink<int> s2 = Stream.CreateSink<int>();
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
            StreamSink<int> s = Stream.CreateSink<int>();
            Stream<int> s2 = s.Map(x => 2 * x);
            List<int> @out = new List<int>();
            IListener l = s.Merge(s2, (x, y) => x + y).Listen(@out.Add);
            s.Send(7);
            s.Send(9);
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 21, 27 }, @out);
        }

        [Test]
        public void TestCoalesce()
        {
            StreamSink<int> s = Stream.CreateSink<int>((x, y) => x + y);
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
            StreamSink<int> s = Stream.CreateSink<int>((x, y) => x + y);
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
            StreamSink<char> s = Stream.CreateSink<char>();
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
            StreamSink<Maybe<string>> s = Stream.CreateSink<Maybe<string>>();
            List<string> @out = new List<string>();
            IListener l = s.FilterMaybe().Listen(@out.Add);
            s.Send(Maybe.Some("tomato"));
            s.Send(Maybe.None);
            s.Send(Maybe.Some("peach"));
            s.Send(Maybe.None);
            s.Send(Maybe.Some("pear"));
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { "tomato", "peach", "pear" }, @out);
        }

        [Test]
        public void TestLoopStream()
        {
            StreamSink<int> sa = Stream.CreateSink<int>();
            (StreamLoop<int> sb, Stream<int> sb2, Stream<int> sc) = Transaction.Run(() =>
            {
                StreamLoop<int> sbLocal = Stream.CreateLoop<int>();
                Stream<int> scLocal = sa.Map(x => x % 10).Merge(sbLocal, (x, y) => x * y);
                Stream<int> sbOut = sa.Map(x => x / 10).Filter(x => x != 0);
                sbLocal.Loop(sbOut);
                return (sbLocal, sbOut, scLocal);
            });
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
            CellSink<int> ca = Cell.CreateSink(22);
            (CellLoop<int> cb, Cell<int> cb2, Cell<int> cc) = Transaction.Run(() =>
            {
                CellLoop<int> cbLocal = Cell.CreateLoop<int>();
                Cell<int> ccLocal = ca.Map(x => x % 10).Lift(cbLocal, (x, y) => x * y);
                Cell<int> cbOut = ca.Map(x => x / 10);
                cbLocal.Loop(cbOut);
                return (cbLocal, cbOut, ccLocal);
            });
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
            StreamSink<char?> sc = Stream.CreateSink<char?>();
            BehaviorSink<bool> cGate = Behavior.CreateSink(true);
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
            StreamSink<int> s = Stream.CreateSink<int>();
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
            StreamSink<int> s = Stream.CreateSink<int>();
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
            StreamSink<int> sa = Stream.CreateSink<int>();
            List<int> @out = new List<int>();
            Stream<int> sum = sa.Collect((Value: 100, Test: true), (a, s) =>
            {
                int outputValue = s.Value + (s.Test ? a * 3 : a);
                return (ReturnValue: outputValue, State: (Value: outputValue, Test: outputValue % 2 == 0));
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
            StreamSink<int> sa = Stream.CreateSink<int>();
            List<int> @out = new List<int>();
            Cell<int> sum = sa.Accum(100, (a, s) => a + s);
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
            StreamSink<char> s = Stream.CreateSink<char>();
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
            StreamSink<char> s = Stream.CreateSink<char>();
            Cell<char> c = s.Hold(' ');
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
            StreamSink<char> s = Stream.CreateSink<char>();
            Cell<char> c = s.Hold(' ');
            List<char> @out = new List<char>();
            IListener l = s.Snapshot(c).Listen(@out.Add);
            s.Send('C');
            s.Send('B');
            s.Send('A');
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { ' ', 'C', 'B' }, @out);
        }

        [Test]
        public void TestDefer()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            Cell<char> c = s.Hold(' ');
            List<char> @out = new List<char>();
            IListener l = Operational.Defer(s).Snapshot(c).Listen(@out.Add);
            s.Send('C');
            s.Send('B');
            s.Send('A');
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 'C', 'B', 'A' }, @out);
        }

        [Test]
        public void TestListenWeak()
        {
            StreamSink<int> s = Stream.CreateSink<int>();

            List<int> @out = new List<int>();

            ((Action)(() =>
            {
                // ReSharper disable once UnusedVariable
                IWeakListener l = s.ListenWeak(@out.Add);

                s.Send(1);
                s.Send(2);
            }))();

            GC.Collect(0, GCCollectionMode.Forced);
            s.Send(3);
            s.Send(4);

            Assert.AreEqual(2, @out.Count);
        }

        [Test]
        public void TestListenWeakWithMap()
        {
            StreamSink<int> s = Stream.CreateSink<int>();

            List<int> @out = new List<int>();

            ((Action)(() =>
            {
                Stream<int> s2 = s.Map(v => v + 1);

                ((Action)(() =>
                {
                    // ReSharper disable once UnusedVariable
                    IWeakListener l = s2.ListenWeak(@out.Add);

                    s.Send(1);
                    s.Send(2);
                }))();

                GC.Collect(0, GCCollectionMode.Forced);

                ((Action)(() =>
                {
                    // ReSharper disable once UnusedVariable
                    IWeakListener l = s2.ListenWeak(@out.Add);

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
            StreamSink<int> s = Stream.CreateSink<int>();

            List<int> @out = new List<int>();

            ((Action)(() =>
            {
                // ReSharper disable once UnusedVariable
                IStrongListener l = s.Listen(@out.Add);

                s.Send(1);

                l.Unlisten();

                s.Send(2);
            }))();

            s.Send(3);
            s.Send(4);

            Assert.AreEqual(1, @out.Count);
        }

        [Test]
        public void TestUnlistenWeak()
        {
            StreamSink<int> s = Stream.CreateSink<int>();

            List<int> @out = new List<int>();

            ((Action)(() =>
            {
                // ReSharper disable once UnusedVariable
                IWeakListener l = s.ListenWeak(@out.Add);

                s.Send(1);

                l.Unlisten();

                s.Send(2);
            }))();

            s.Send(3);
            s.Send(4);

            Assert.AreEqual(1, @out.Count);
        }

        [Test]
        public void TestMultipleUnlisten()
        {
            StreamSink<int> s = Stream.CreateSink<int>();

            List<int> @out = new List<int>();

            ((Action)(() =>
            {
                // ReSharper disable once UnusedVariable
                IStrongListener l = s.Listen(@out.Add);

                s.Send(1);

                l.Unlisten();
                l.Unlisten();

                s.Send(2);

                l.Unlisten();
            }))();

            s.Send(3);
            s.Send(4);

            Assert.AreEqual(1, @out.Count);
        }

        [Test]
        public void TestMultipleUnlistenWeak()
        {
            StreamSink<int> s = Stream.CreateSink<int>();

            List<int> @out = new List<int>();

            ((Action)(() =>
            {
                // ReSharper disable once UnusedVariable
                IWeakListener l = s.ListenWeak(@out.Add);

                s.Send(1);

                l.Unlisten();
                l.Unlisten();

                s.Send(2);

                l.Unlisten();
            }))();

            s.Send(3);
            s.Send(4);

            Assert.AreEqual(1, @out.Count);
        }

        [Test]
        public void TestListenOnce()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            List<char> @out = new List<char>();
            IListener l = s.ListenOnce(@out.Add);
            s.Send('A');
            s.Send('B');
            s.Send('C');
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 'A' }, @out);
        }

        [Test]
        public async Task TestListenOnceAsync()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            new Thread(() =>
            {
                Thread.Sleep(250);
                s.Send('A');
                s.Send('B');
                s.Send('C');
            }).Start();
            char r = await s.ListenOnceAsync();
            Assert.AreEqual('A', r);
        }

        [Test]
        public async Task TestListenOnceAsyncWithCleanup()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            new Thread(() =>
            {
                Thread.Sleep(250);
                s.Send('A');
                s.Send('B');
                s.Send('C');
            }).Start();
            TaskWithListener<char> t = s.ListenOnceAsync();
            GC.Collect(0, GCCollectionMode.Forced);
            char r = await t;
            Assert.AreEqual('A', r);
        }

        [Test]
        public async Task TestListenOnceAsyncSameThread()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            TaskWithListener<char> t = s.ListenOnceAsync();
            s.Send('A');
            s.Send('B');
            s.Send('C');
            char r = await t;
            Assert.AreEqual('A', r);
        }

        [Test]
        public async Task TestListenOnceAsyncSameThreadWithCleanup()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            TaskWithListener<char> t = s.ListenOnceAsync();
            GC.Collect(0, GCCollectionMode.Forced);
            s.Send('A');
            s.Send('B');
            s.Send('C');
            char r = await t;
            Assert.AreEqual('A', r);
        }

        [Test]
        public async Task TestListenOnceAsyncModify()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            TaskWithListener<char> t = s.ListenOnceAsync(async t2 => await t2.ConfigureAwait(false));
            GC.Collect(0, GCCollectionMode.Forced);
            s.Send('A');
            s.Send('B');
            s.Send('C');
            char r = await t;
            Assert.AreEqual('A', r);
        }

        [Test]
        public async Task TestListenOnceAsyncModifyVoid()
        {
            char r = ' ';
            void SetResult(char v) => r = v;
            StreamSink<char> s = Stream.CreateSink<char>();
            TaskWithListener t = s.ListenOnceAsync(t2 => t2.ContinueWith(t3 => SetResult(t3.Result), TaskContinuationOptions.ExecuteSynchronously));
            GC.Collect(0, GCCollectionMode.Forced);
            s.Send('A');
            s.Send('B');
            s.Send('C');
            await t;
            Assert.AreEqual('A', r);
        }

        [Test]
        public async Task TestListenOnceAsyncModifyVoid2()
        {
            char r = ' ';
            StreamSink<char> s = Stream.CreateSink<char>();
            TaskWithListener t = s.ListenOnceAsync(t2 => t2.ContinueWith(t3 => r = t3.Result, TaskContinuationOptions.ExecuteSynchronously));
            GC.Collect(0, GCCollectionMode.Forced);
            s.Send('A');
            s.Send('B');
            s.Send('C');
            await t;
            Assert.AreEqual('A', r);
        }

        [Test]
        public async Task TestListenAsync()
        {
            CellSink<int> a = Cell.CreateSink(1);
            Cell<int> a1 = a.Map(x => x + 1);
            Cell<int> a2 = a.Map(x => x * 2);
            (List<int> results, CellLoop<int> called, IListener l) = Transaction.Run(() =>
             {
                 Cell<int> result = a1.Lift(a2, (x, y) => x + y);
                 Stream<Unit> incrementStream = result.Values().MapTo(Unit.Value);
                 StreamSink<Unit> decrementStream = Stream.CreateSink<Unit>();
                 CellLoop<int> calledLoop = Cell.CreateLoop<int>();
                 calledLoop.Loop(incrementStream.MapTo(1).Merge(decrementStream.MapTo(-1), (x, y) => x + y).Snapshot(calledLoop, (u, c) => c + u).Hold(0));
                 List<int> r = new List<int>();
                 IListener lLocal = result.Listen(v =>
                 {
                     Task.Run(async () =>
                     {
                         await Task.Delay(900);
                         r.Add(v);
                         decrementStream.Send(Unit.Value);
                     });
                 });
                 return (r, calledLoop, lLocal);
             });
            // ReSharper disable once UnusedVariable
            List<int> calledResults = new List<int>();
            IListener l2 = called.Listen(calledResults.Add);

            await Task.Delay(500);
            a.Send(2);
            await Task.Delay(500);
            a.Send(3);
            await Task.Delay(2500);

            l2.Unlisten();
            l.Unlisten();
        }

        [Test]
        public void TestStreamLoop()
        {
            StreamSink<int> streamSink = Stream.CreateSink<int>();
            Stream<int> s = Transaction.Run(() =>
            {
                StreamLoop<int> sl = new StreamLoop<int>();
                Cell<int> c = sl.Map(v => v + 2).Hold(0);
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
        public void TestStreamLoopDefer()
        {
            StreamSink<int> streamSink = Stream.CreateSink<int>();
            Stream<int> stream = Transaction.Run(() =>
            {
                StreamLoop<int> streamLoop = new StreamLoop<int>();
                Stream<int> streamLocal = Operational.Defer(streamSink.OrElse(streamLoop).Filter(v => v < 5).Map(v => v + 1));
                streamLoop.Loop(streamLocal);
                return streamLocal;
            });
            List<int> @out = new List<int>();
            IListener l = stream.Listen(@out.Add);
            streamSink.Send(2);
            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 3, 4, 5 }, @out);
        }
    }
}