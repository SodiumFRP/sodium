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
            StreamSink<int> s = new StreamSink<int>();
            Cell<int> c = s.Hold(0);
            List<int> @out = new List<int>();
            using (c.Listen(@out.Add))
            {
                s.Send(2);
                s.Send(9);
            }
            CollectionAssert.AreEqual(new[] { 0, 2, 9 }, @out);
        }

        [Test]
        public void TestHoldUpdates()
        {
            StreamSink<int> s = new StreamSink<int>();
            Cell<int> c = s.Hold(0);
            List<int> @out = new List<int>();
            using (Operational.Updates(c).Listen(@out.Add))
            {
                s.Send(2);
                s.Send(9);
            }
            CollectionAssert.AreEqual(new[] { 2, 9 }, @out);
        }

        [Test]
        public void TestSnapshot()
        {
            CellSink<int> c = new CellSink<int>(0);
            StreamSink<long> trigger = new StreamSink<long>();
            List<string> @out = new List<string>();
            using (trigger.Snapshot(c, (x, y) => x + " " + y).Listen(@out.Add))
            {
                trigger.Send(100L);
                c.Send(2);
                trigger.Send(200L);
                c.Send(9);
                c.Send(1);
                trigger.Send(300L);
            }
            CollectionAssert.AreEqual(new[] { "100 0", "200 2", "300 1" }, @out);
        }

        [Test]
        public void TestListen()
        {
            CellSink<int> c = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (c.Listen(@out.Add))
            {
                c.Send(2);
                c.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 9, 2, 7 }, @out);
        }

        [Test]
        public void TestListenOnce()
        {
            CellSink<int> c = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(c).ListenOnce(@out.Add)))
            {
                c.Send(2);
                c.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 9 }, @out);
        }

        [Test]
        public async Task TestListenOnceTask()
        {
            CellSink<int> c = new CellSink<int>(9);
            int result = await Transaction.Run(() => Operational.Value(c).ListenOnce());
            c.Send(2);
            c.Send(7);
            Assert.AreEqual(9, result);
        }

        [Test]
        public void TestUpdates()
        {
            CellSink<int> c = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Operational.Updates(c).Listen(@out.Add))
            {
                c.Send(2);
                c.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestValue()
        {
            CellSink<int> c = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(c).Listen(@out.Add)))
            {
                c.Send(2);
                c.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 9, 2, 7 }, @out);
        }

        [Test]
        public void TestValueThenMap()
        {
            CellSink<int> c = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(c).Map(x => x + 100).Listen(@out.Add)))
            {
                c.Send(2);
                c.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 109, 102, 107 }, @out);
        }

        [Test]
        public void TestValueThenMerge()
        {
            CellSink<int> c1 = new CellSink<int>(9);
            CellSink<int> c2 = new CellSink<int>(2);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(c1).Merge(Operational.Value(c2), (x, y) => x + y).Listen(@out.Add)))
            {
                c1.Send(1);
                c2.Send(4);
            }
            CollectionAssert.AreEqual(new[] { 11, 1, 4 }, @out);
        }

        [Test]
        public void TestValueThenFilter()
        {
            CellSink<int> c = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(c).Filter(x => x % 2 != 0).Listen(@out.Add)))
            {
                c.Send(2);
                c.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 9, 7 }, @out);
        }

        [Test]
        public void TestValueThenOnce()
        {
            CellSink<int> c = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(c).Once().Listen(@out.Add)))
            {
                c.Send(2);
                c.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 9 }, @out);
        }

        [Test]
        public void TestValueThenLateListen()
        {
            CellSink<int> c = new CellSink<int>(9);
            List<int> @out = new List<int>();
            Stream<int> value = Operational.Value(c);
            c.Send(8);
            using (value.Listen(@out.Add))
            {
                c.Send(2);
                c.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestMap()
        {
            CellSink<int> c = new CellSink<int>(6);
            List<string> @out = new List<string>();
            using (c.Map(x => x.ToString()).Listen(@out.Add))
            {
                c.Send(8);
            }
            CollectionAssert.AreEqual(new[] { "6", "8" }, @out);
        }

        [Test]
        public void TestMapLateListen()
        {
            CellSink<int> c = new CellSink<int>(6);
            List<string> @out = new List<string>();
            Cell<string> cm = c.Map(x => x.ToString());
            c.Send(2);
            using (cm.Listen(@out.Add))
            {
                c.Send(8);
            }
            CollectionAssert.AreEqual(new[] { "2", "8" }, @out);
        }

        [Test]
        public void TestCalm()
        {
            CellSink<int> c = new CellSink<int>(2);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => c.Calm().Listen(@out.Add)))
            {
                c.Send(2);
                c.Send(2);
                c.Send(4);
                c.Send(2);
                c.Send(4);
                c.Send(4);
                c.Send(2);
                c.Send(2);
            }
            CollectionAssert.AreEqual(new[] { 2, 4, 2, 4, 2 }, @out);
        }

        [Test]
        public void TestCalm2()
        {
            CellSink<int> c = new CellSink<int>(2);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => c.Calm().Listen(@out.Add)))
            {
                c.Send(4);
                c.Send(2);
                c.Send(4);
                c.Send(4);
                c.Send(2);
                c.Send(2);
            }
            CollectionAssert.AreEqual(new[] { 2, 4, 2, 4, 2 }, @out);
        }

        [Test]
        public void TestApply()
        {
            CellSink<Func<long, string>> cf = new CellSink<Func<long, string>>(x => "1 " + x);
            CellSink<long> ca = new CellSink<long>(5L);
            List<string> @out = new List<string>();
            using (ca.Apply(cf).Listen(@out.Add))
            {
                cf.Send(x => "12 " + x);
                ca.Send(6L);
            }
            CollectionAssert.AreEqual(new[] { "1 5", "12 5", "12 6" }, @out);
        }

        [Test]
        public void TestLift()
        {
            CellSink<int> c1 = new CellSink<int>(1);
            CellSink<long> c2 = new CellSink<long>(5L);
            List<string> @out = new List<string>();
            using (c1.Lift(c2, (x, y) => x + " " + y).Listen(@out.Add))
            {
                c1.Send(12);
                c2.Send(6L);
            }
            CollectionAssert.AreEqual(new[] { "1 5", "12 5", "12 6" }, @out);
        }

        [Test]
        public void TestLiftGlitch()
        {
            CellSink<int> c1 = new CellSink<int>(1);
            Cell<int> c3 = c1.Map(x => x * 3);
            Cell<int> c5 = c1.Map(x => x * 5);
            Cell<string> c = c3.Lift(c5, (x, y) => x + " " + y);
            List<string> @out = new List<string>();
            using (c.Listen(@out.Add))
            {
                c1.Send(2);
            }
            CollectionAssert.AreEqual(new[] { "3 5", "6 10" }, @out);
        }

        [Test]
        public void TestLiftFromSimultaneous()
        {
            Tuple<CellSink<int>, CellSink<int>> t = Transaction.Run(() =>
            {
                CellSink<int> localC1 = new CellSink<int>(3);
                CellSink<int> localC2 = new CellSink<int>(5);
                localC2.Send(7);
                return Tuple.Create(localC1, localC2);
            });
            CellSink<int> c1 = t.Item1;
            CellSink<int> c2 = t.Item2;
            List<int> @out = new List<int>();
            using (c1.Lift(c2, (x, y) => x + y).Listen(@out.Add))
            {
            }
            CollectionAssert.AreEqual(new[] { 10 }, @out);
        }

        [Test]
        public void TestHoldIsDelayed()
        {
            StreamSink<int> s = new StreamSink<int>();
            Cell<int> h = s.Hold(0);
            Stream<string> pair = s.Snapshot(h, (a, b) => a + " " + b);
            List<string> @out = new List<string>();
            using (pair.Listen(@out.Add))
            {
                s.Send(2);
                s.Send(3);
            }
            CollectionAssert.AreEqual(new[] { "2 0", "3 2" }, @out);
        }

        private class Sc
        {
            public readonly IMaybe<char> A;
            public readonly IMaybe<char> B;
            public readonly IMaybe<Cell<char>> Sw;

            public Sc(IMaybe<char> a, IMaybe<char> b, IMaybe<Cell<char>> sw)
            {
                this.A = a;
                this.B = b;
                this.Sw = sw;
            }
        }

        [Test]
        public void TestSwitchC()
        {
            StreamSink<Sc> ssc = new StreamSink<Sc>();
            // Split each field out of SB so we can update multiple behaviors in a
            // single transaction.
            Cell<char> ca = ssc.Map(s => s.A).FilterMaybe().Hold('A');
            Cell<char> cb = ssc.Map(s => s.B).FilterMaybe().Hold('a');
            Cell<Cell<char>> csw = ssc.Map(s => s.Sw).FilterMaybe().Hold(ca);
            Cell<char> co = csw.SwitchC();
            List<char> @out = new List<char>();
            using (co.Listen(@out.Add))
            {
                ssc.Send(new Sc(Maybe.Just('B'), Maybe.Just('b'), Maybe.Nothing<Cell<char>>()));
                ssc.Send(new Sc(Maybe.Just('C'), Maybe.Just('c'), Maybe.Just(cb)));
                ssc.Send(new Sc(Maybe.Just('D'), Maybe.Just('d'), Maybe.Nothing<Cell<char>>()));
                ssc.Send(new Sc(Maybe.Just('E'), Maybe.Just('e'), Maybe.Just(ca)));
                ssc.Send(new Sc(Maybe.Just('F'), Maybe.Just('f'), Maybe.Nothing<Cell<char>>()));
                ssc.Send(new Sc(Maybe.Nothing<char>(), Maybe.Nothing<char>(), Maybe.Just(cb)));
                ssc.Send(new Sc(Maybe.Nothing<char>(), Maybe.Nothing<char>(), Maybe.Just(ca)));
                ssc.Send(new Sc(Maybe.Just('G'), Maybe.Just('g'), Maybe.Just(cb)));
                ssc.Send(new Sc(Maybe.Just('H'), Maybe.Just('h'), Maybe.Just(ca)));
                ssc.Send(new Sc(Maybe.Just('I'), Maybe.Just('i'), Maybe.Just(ca)));
            }
            CollectionAssert.AreEqual(new[] { 'A', 'B', 'c', 'd', 'E', 'F', 'f', 'F', 'g', 'H', 'I' }, @out);
        }

        private class Sc2
        {
            public readonly CellSink<int> C;

            public Sc2(int initialValue)
            {
                this.C = new CellSink<int>(initialValue);
            }
        }

        [Test]
        public void TestSwitchCSimultaneous()
        {
            Sc2 sc1 = new Sc2(0);
            CellSink<Sc2> csc = new CellSink<Sc2>(sc1);
            Cell<int> co = csc.Map<Cell<int>>(b => b.C).SwitchC();
            List<int> @out = new List<int>();
            using (co.Listen(@out.Add))
            {
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
            }
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
            StreamSink<Ss> sss = new StreamSink<Ss>();
            // Split each field out of SB so we can update multiple behaviors in a
            // single transaction.
            Stream<char> sa = sss.Map(s => s.A);
            Stream<char> sb = sss.Map(s => s.B);
            Cell<Stream<char>> csw = sss.Map(s => s.Sw).FilterMaybe().Hold(sa);
            Stream<char> so = csw.SwitchS();
            List<char> @out = new List<char>();
            using (so.Listen(@out.Add))
            {
                sss.Send(new Ss('A', 'a', Maybe.Nothing<Stream<char>>()));
                sss.Send(new Ss('B', 'b', Maybe.Nothing<Stream<char>>()));
                sss.Send(new Ss('C', 'c', Maybe.Just(sb)));
                sss.Send(new Ss('D', 'd', Maybe.Nothing<Stream<char>>()));
                sss.Send(new Ss('E', 'e', Maybe.Just(sa)));
                sss.Send(new Ss('F', 'f', Maybe.Nothing<Stream<char>>()));
                sss.Send(new Ss('G', 'g', Maybe.Just(sb)));
                sss.Send(new Ss('H', 'h', Maybe.Just(sa)));
                sss.Send(new Ss('I', 'i', Maybe.Just(sa)));
            }
            CollectionAssert.AreEqual(new[] { 'A', 'B', 'C', 'd', 'e', 'F', 'G', 'h', 'I' }, @out);
        }

        private class Ss2
        {
            public readonly StreamSink<int> S = new StreamSink<int>();
        }

        [Test]
        public void TestSwitchSSimultaneous()
        {
            Ss2 ss1 = new Ss2();
            CellSink<Ss2> css = new CellSink<Ss2>(ss1);
            Stream<int> so = css.Map<Stream<int>>(b => b.S).SwitchS();
            List<int> @out = new List<int>();
            using (so.Listen(@out.Add))
            {
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
            }
            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, @out);
        }

        [Test]
        public void TestSwitchEarlyS()
        {
            StreamSink<Ss> sss = new StreamSink<Ss>();
            // Split each field out of SB so we can update multiple behaviors in a
            // single transaction.
            Stream<char> sa = sss.Map(s => s.A);
            Stream<char> sb = sss.Map(s => s.B);
            Cell<Stream<char>> csw = sss.Map(s => s.Sw).FilterMaybe().Hold(sa);
            Stream<char> so = csw.SwitchEarlyS();
            List<char> @out = new List<char>();
            using (so.Listen(@out.Add))
            {
                sss.Send(new Ss('A', 'a', Maybe.Nothing<Stream<char>>()));
                sss.Send(new Ss('B', 'b', Maybe.Nothing<Stream<char>>()));
                sss.Send(new Ss('C', 'c', Maybe.Just(sb)));
                sss.Send(new Ss('D', 'd', Maybe.Nothing<Stream<char>>()));
                sss.Send(new Ss('E', 'e', Maybe.Just(sa)));
                sss.Send(new Ss('F', 'f', Maybe.Nothing<Stream<char>>()));
                sss.Send(new Ss('G', 'g', Maybe.Just(sb)));
                sss.Send(new Ss('H', 'h', Maybe.Just(sa)));
                sss.Send(new Ss('I', 'i', Maybe.Just(sa)));
            }
            CollectionAssert.AreEqual(new[] { 'A', 'B', 'c', 'd', 'E', 'F', 'g', 'H', 'I' }, @out);
        }

        [Test]
        public void TestSwitchEarlySSimultaneous()
        {
            Ss2 ss1 = new Ss2();
            CellSink<Ss2> css = new CellSink<Ss2>(ss1);
            Stream<int> so = css.Map<Stream<int>>(b => b.S).SwitchEarlyS();
            List<int> @out = new List<int>();
            using (so.Listen(@out.Add))
            {
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
            }
            CollectionAssert.AreEqual(new[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, @out);
        }

        [Test]
        public void TestLiftList()
        {
            IReadOnlyList<CellSink<int>> cellSinks = Enumerable.Range(0, 50).Select(_ => new CellSink<int>(1)).ToArray();
            Cell<int> sum = cellSinks.Lift(v => v.Sum());
            List<int> @out = new List<int>();
            using (sum.Listen(@out.Add))
            {
                cellSinks[4].Send(5);
                cellSinks[5].Send(5);
                Transaction.RunVoid(() =>
                {
                    cellSinks[9].Send(5);
                    cellSinks[17].Send(5);
                    cellSinks[41].Send(5);
                    cellSinks[48].Send(5);
                });
            }
            CollectionAssert.AreEqual(new[] { 50, 54, 58, 74 }, @out);
        }

        [Test]
        public void TestLiftListLarge()
        {
            IReadOnlyList<CellSink<int>> cellSinks = Enumerable.Range(0, 500).Select(_ => new CellSink<int>(1)).ToArray();
            Cell<int> sum = cellSinks.Lift(v => v.Sum());
            List<int> @out = new List<int>();
            using (sum.Listen(@out.Add))
            {
                cellSinks[4].Send(5);
                cellSinks[5].Send(5);
                Transaction.RunVoid(() =>
                {
                    cellSinks[9].Send(5);
                    cellSinks[17].Send(5);
                    cellSinks[41].Send(5);
                    cellSinks[48].Send(5);
                });
            }
            CollectionAssert.AreEqual(new[] { 500, 504, 508, 524 }, @out);
        }

        [Test]
        public void TestLiftListLargeManyUpdates()
        {
            IReadOnlyList<CellSink<int>> cellSinks = Enumerable.Range(0, 500).Select(_ => new CellSink<int>(1)).ToArray();
            Cell<int> sum = cellSinks.Lift(v => v.Sum());
            List<int> @out = new List<int>();
            using (sum.Listen(@out.Add))
            {
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
            }
            IReadOnlyList<int> expected = new[] { 500 }.Concat(Enumerable.Range(0, 100).SelectMany(n => new[] { 500 + 20 * n + 4, 500 + 20 * n + 8, 500 + 20 * n + 20 })).ToArray();
            CollectionAssert.AreEqual(expected, @out);
        }

        [Test]
        public void TestLiftListChangesWhileListening()
        {
            IReadOnlyList<CellSink<int>> cellSinks = Enumerable.Range(0, 50).Select(_ => new CellSink<int>(1)).ToArray();
            Cell<int> sum = cellSinks.Lift(v => v.Sum());
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
    }
}