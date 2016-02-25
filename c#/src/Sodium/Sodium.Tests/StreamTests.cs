using System;
using System.Collections.Generic;
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
            using (s.Listen(@out.Add))
            {
                s.Send(5);
            }
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
            using (m.Listen(@out.Add))
            {
                s.Send(5);
                s.Send(3);
            }
            CollectionAssert.AreEqual(new[] { "7", "5" }, @out);
        }

        [Test]
        public void TestOrElseNonSimultaneous()
        {
            StreamSink<int> s1 = new StreamSink<int>();
            StreamSink<int> s2 = new StreamSink<int>();
            List<int> @out = new List<int>();
            using (s1.OrElse(s2).Listen(@out.Add))
            {
                s1.Send(7);
                s2.Send(9);
                s1.Send(8);
            }
            CollectionAssert.AreEqual(new[] { 7, 9, 8 }, @out);
        }

        [Test]
        public void TestOrElseSimultaneous1()
        {
            StreamSink<int> s1 = new StreamSink<int>((l, r) => r);
            StreamSink<int> s2 = new StreamSink<int>((l, r) => r);
            List<int> @out = new List<int>();
            using (s2.OrElse(s1).Listen(@out.Add))
            {
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
            }
            CollectionAssert.AreEqual(new[] { 60, 9, 90, 90, 90 }, @out);
        }

        [Test]
        public void TestOrElseSimultaneous2()
        {
            StreamSink<int> s = new StreamSink<int>();
            Stream<int> s2 = s.Map(x => 2 * x);
            List<int> @out = new List<int>();
            using (s.OrElse(s2).Listen(@out.Add))
            {
                s.Send(7);
                s.Send(9);
            }
            CollectionAssert.AreEqual(new[] { 7, 9 }, @out);
        }

        [Test]
        public void TestOrElseLeftBias()
        {
            StreamSink<int> s = new StreamSink<int>();
            Stream<int> s2 = s.Map(x => 2 * x);
            List<int> @out = new List<int>();
            using (s2.OrElse(s).Listen(@out.Add))
            {
                s.Send(7);
                s.Send(9);
            }
            CollectionAssert.AreEqual(new[] { 14, 18 }, @out);
        }

        [Test]
        public void TestMergeNonSimultaneous()
        {
            StreamSink<int> s1 = new StreamSink<int>();
            StreamSink<int> s2 = new StreamSink<int>();
            List<int> @out = new List<int>();
            using (s1.Merge(s2, (x, y) => x + y).Listen(@out.Add))
            {
                s1.Send(7);
                s2.Send(9);
                s1.Send(8);
            }
            CollectionAssert.AreEqual(new[] { 7, 9, 8 }, @out);
        }

        [Test]
        public void TestMergeSimultaneous()
        {
            StreamSink<int> s = new StreamSink<int>();
            Stream<int> s2 = s.Map(x => 2 * x);
            List<int> @out = new List<int>();
            using (s.Merge(s2, (x, y) => x + y).Listen(@out.Add))
            {
                s.Send(7);
                s.Send(9);
            }
            CollectionAssert.AreEqual(new[] { 21, 27 }, @out);
        }

        [Test]
        public void TestCoalesce1()
        {
            StreamSink<int> s = new StreamSink<int>((x, y) => x + y);
            List<int> @out = new List<int>();
            using (s.Listen(@out.Add))
            {
                Transaction.RunVoid(() =>
                {
                    s.Send(2);
                });
                Transaction.RunVoid(() =>
                {
                    s.Send(8);
                    s.Send(40);
                });
            }
            CollectionAssert.AreEqual(new[] { 2, 48 }, @out.ToArray());
        }

        [Test]
        public void TestCoalesce2()
        {
            StreamSink<int> s = new StreamSink<int>((x, y) => x + y);
            List<int> @out = new List<int>();
            using (s.Listen(@out.Add))
            {
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
            }
            CollectionAssert.AreEqual(new[] { 15, 40 }, @out.ToArray());
        }

        [Test]
        public void TestFilter()
        {
            StreamSink<char> s = new StreamSink<char>();
            List<char> @out = new List<char>();
            using (s.Filter(char.IsUpper).Listen(c => { @out.Add(c); }))
            {
                s.Send('H');
                s.Send('o');
                s.Send('I');
            }
            CollectionAssert.AreEqual(new[] { 'H', 'I' }, @out);
        }

        [Test]
        public void TestFilterMaybe()
        {
            StreamSink<IMaybe<string>> e = new StreamSink<IMaybe<string>>();
            List<string> @out = new List<string>();
            using (e.FilterMaybe().Listen(s => { @out.Add(s); }))
            {
                e.Send(Maybe.Just("tomato"));
                e.Send(Maybe.Nothing<string>());
                e.Send(Maybe.Just("peach"));
                e.Send(Maybe.Just<string>(null));
                e.Send(Maybe.Just("pear"));
            }
            CollectionAssert.AreEqual(new[] { "tomato", "peach", null, "pear" }, @out);
        }

        [Test]
        public void TestLoopStream()
        {
            StreamSink<int> sa = new StreamSink<int>();
            Stream<int> sc = Transaction.Run(() =>
            {
                StreamLoop<int> sb = new StreamLoop<int>();
                Stream<int> scLocal = sa.Map(x => x % 10).Merge(sb, (x, y) => x * y);
                Stream<int> sbOut = sa.Map(x => x / 10).Filter(x => x != 0);
                sb.Loop(sbOut);
                return scLocal;
            });
            List<int> @out = new List<int>();
            using (sc.Listen(@out.Add))
            {
                sa.Send(2);
                sa.Send(52);
            }
            CollectionAssert.AreEqual(new[] { 2, 10 }, @out.ToArray());
        }

        [Test]
        public void TestGate()
        {
            StreamSink<char?> sc = new StreamSink<char?>();
            CellSink<bool> cGate = new CellSink<bool>(true);
            List<char?> @out = new List<char?>();
            using (sc.Gate(cGate).Listen(@out.Add))
            {
                sc.Send('H');
                cGate.Send(false);
                sc.Send('O');
                cGate.Send(true);
                sc.Send('I');
            }
            CollectionAssert.AreEqual(new[] { 'H', 'I' }, @out);
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
            using (sum.Listen(@out.Add))
            {
                sa.Send(5);
                sa.Send(7);
                sa.Send(1);
                sa.Send(2);
                sa.Send(3);
            }
            CollectionAssert.AreEqual(new[] { 115, 122, 125, 127, 130 }, @out);
        }

        [Test]
        public void TestAccum()
        {
            StreamSink<int> sa = new StreamSink<int>();
            List<int> @out = new List<int>();
            Cell<int> sum = sa.Accum(100, (a, s) => a + s);
            using (sum.Listen(@out.Add))
            {
                sa.Send(5);
                sa.Send(7);
                sa.Send(1);
                sa.Send(2);
                sa.Send(3);
            }
            CollectionAssert.AreEqual(new[] { 100, 105, 112, 113, 115, 118 }, @out);
        }

        [Test]
        public void TestOnce()
        {
            StreamSink<char> s = new StreamSink<char>();
            List<char> @out = new List<char>();
            using (s.Once().Listen(@out.Add))
            {
                s.Send('A');
                s.Send('B');
                s.Send('C');
            }
            CollectionAssert.AreEqual(new[] { 'A' }, @out);
        }

        [Test]
        public void TestHold()
        {
            StreamSink<char> s = new StreamSink<char>();
            Cell<char> b = s.Hold(' ');
            List<char> @out = new List<char>();
            using (b.Listen(@out.Add))
            {
                s.Send('C');
                s.Send('B');
                s.Send('A');
            }
            CollectionAssert.AreEqual(new[] { ' ', 'C', 'B', 'A' }, @out);
        }

        [Test]
        public void TestHoldImplicitDelay()
        {
            StreamSink<char> s = new StreamSink<char>();
            Cell<char> b = s.Hold(' ');
            List<char> @out = new List<char>();
            using (s.Snapshot(b).Listen(@out.Add))
            {
                s.Send('C');
                s.Send('B');
                s.Send('A');
            }
            CollectionAssert.AreEqual(new[] { ' ', 'C', 'B' }, @out);
        }

        [Test]
        public void TestDefer()
        {
            StreamSink<char> s = new StreamSink<char>();
            Cell<char> b = s.Hold(' ');
            List<char> @out = new List<char>();
            using (Operational.Defer(s).Snapshot(b).Listen(@out.Add))
            {
                s.Send('C');
                s.Send('B');
                s.Send('A');
            }
            CollectionAssert.AreEqual(new[] { 'C', 'B', 'A' }, @out);
        }
    }
}