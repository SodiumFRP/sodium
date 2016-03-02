using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class CellTests
    {
        [Test]
        public void TestHold()
        {
            StreamSink<int> e = new StreamSink<int>();
            Cell<int> b = e.Hold(0);
            List<int> @out = new List<int>();
            using (b.Listen(@out.Add))
            {
                e.Send(2);
                e.Send(9);
            }
            CollectionAssert.AreEqual(new[] { 0, 2, 9 }, @out);
        }

        [Test]
        public void TestHoldUpdates()
        {
            StreamSink<int> e = new StreamSink<int>();
            Cell<int> b = e.Hold(0);
            List<int> @out = new List<int>();
            using (Operational.Updates(b).Listen(@out.Add))
            {
                e.Send(2);
                e.Send(9);
            }
            CollectionAssert.AreEqual(new[] { 2, 9 }, @out);
        }

        [Test]
        public void TestSnapshot()
        {
            CellSink<int> b = new CellSink<int>(0);
            StreamSink<long> trigger = new StreamSink<long>();
            List<string> @out = new List<string>();
            using (trigger.Snapshot(b, (x, y) => x + " " + y).Listen(@out.Add))
            {
                trigger.Send(100L);
                b.Send(2);
                trigger.Send(200L);
                b.Send(9);
                b.Send(1);
                trigger.Send(300L);
            }
            CollectionAssert.AreEqual(new[] { "100 0", "200 2", "300 1" }, @out);
        }

        [Test]
        public void TestListen()
        {
            CellSink<int> b = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (b.Listen(@out.Add))
            {
                b.Send(2);
                b.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 9, 2, 7 }, @out);
        }

        [Test]
        public void TestUpdates()
        {
            CellSink<int> b = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Operational.Updates(b).Listen(@out.Add))
            {
                b.Send(2);
                b.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 2, 7 }, @out);
        }

        [Test]
        public void TestValue()
        {
            CellSink<int> b = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(b).Listen(@out.Add)))
            {
                b.Send(2);
                b.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 9, 2, 7 }, @out);
        }

        [Test]
        public void TestValueThenMap()
        {
            CellSink<int> b = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(b).Map(x => x + 100).Listen(@out.Add)))
            {
                b.Send(2);
                b.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 109, 102, 107 }, @out);
        }

        [Test]
        public void TestValueThenMerge()
        {
            CellSink<int> b1 = new CellSink<int>(9);
            CellSink<int> b2 = new CellSink<int>(2);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(b1).Merge(Operational.Value(b2), (x, y) => x + y).Listen(@out.Add)))
            {
                b1.Send(1);
                b2.Send(4);
            }
            CollectionAssert.AreEqual(new[] { 11, 1, 4 }, @out);
        }

        [Test]
        public void TestValueThenFilter()
        {
            CellSink<int> b = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(b).Filter(_ => true).Listen(@out.Add)))
            {
                b.Send(2);
                b.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 9, 2, 7 }, @out);
        }

        [Test]
        public void TestValueThenOnce()
        {
            CellSink<int> b = new CellSink<int>(9);
            List<int> @out = new List<int>();
            using (Transaction.Run(() => Operational.Value(b).Once().Listen(@out.Add)))
            {
                b.Send(2);
                b.Send(7);
            }
            CollectionAssert.AreEqual(new[] { 9 }, @out);
        }

        [Test]
        public void TestValueThenLateListen()
        {
            CellSink<int> b = new CellSink<int>(9);
            List<int> @out = new List<int>();
            Stream<int> value = Operational.Value(b);
            b.Send(8);
            using (value.Listen(@out.Add))
            {
                b.Send(2);
            }
            CollectionAssert.AreEqual(new[] { 2 }, @out);
        }

        [Test]
        public void TestMap()
        {
            CellSink<int> b = new CellSink<int>(6);
            List<string> @out = new List<string>();
            using (b.Map(x => x.ToString()).Listen(@out.Add))
            {
                b.Send(8);
            }
            CollectionAssert.AreEqual(new[] { "6", "8" }, @out);
        }

        [Test]
        public void TestMapLateListen()
        {
            CellSink<int> b = new CellSink<int>(6);
            List<string> @out = new List<string>();
            Cell<string> bm = b.Map(x => x.ToString());
            b.Send(2);
            using (bm.Listen(@out.Add))
            {
                b.Send(8);
            }
            CollectionAssert.AreEqual(new[] { "2", "8" }, @out);
        }

        [Test]
        public void TestApply()
        {
            CellSink<Func<long, string>> bf = new CellSink<Func<long, string>>(b => "1 " + b);
            CellSink<long> ba = new CellSink<long>(5L);
            List<string> @out = new List<string>();
            using (ba.Apply(bf).Listen(@out.Add))
            {
                bf.Send(b => "12 " + b);
                ba.Send(6L);
            }
            CollectionAssert.AreEqual(new[] { "1 5", "12 5", "12 6" }, @out);
        }

        [Test]
        public void TestLift()
        {
            CellSink<int> b1 = new CellSink<int>(1);
            CellSink<long> b2 = new CellSink<long>(5L);
            List<string> @out = new List<string>();
            using (b1.Lift(b2, (x, y) => x + " " + y).Listen(@out.Add))
            {
                b1.Send(12);
                b2.Send(6L);
            }
            CollectionAssert.AreEqual(new[] { "1 5", "12 5", "12 6" }, @out);
        }

        [Test]
        public void TestLiftGlitch()
        {
            CellSink<int> b1 = new CellSink<int>(1);
            Cell<int> b3 = b1.Map(x => x * 3);
            Cell<int> b5 = b1.Map(x => x * 5);
            Cell<string> b = b3.Lift(b5, (x, y) => x + " " + y);
            List<string> @out = new List<string>();
            using (b.Listen(@out.Add))
            {
                b1.Send(2);
            }
            CollectionAssert.AreEqual(new[] { "3 5", "6 10" }, @out);
        }

        [Test]
        public void TestLiftFromSimultaneous()
        {
            Tuple<CellSink<int>, CellSink<int>> t = Transaction.Run(() =>
            {
                CellSink<int> localB1 = new CellSink<int>(3);
                CellSink<int> localB2 = new CellSink<int>(5);
                localB2.Send(7);
                return Tuple.Create(localB1, localB2);
            });
            CellSink<int> b1 = t.Item1;
            CellSink<int> b2 = t.Item2;
            List<int> @out = new List<int>();
            using (b1.Lift(b2, (x, y) => x + y).Listen(@out.Add))
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
            StreamSink<Sc> esb = new StreamSink<Sc>();
            // Split each field out of SB so we can update multiple behaviors in a
            // single transaction.
            Cell<char> ba = esb.Map(s => s.A).FilterMaybe().Hold('A');
            Cell<char> bb = esb.Map(s => s.B).FilterMaybe().Hold('a');
            Cell<Cell<char>> bsw = esb.Map(s => s.Sw).FilterMaybe().Hold(ba);
            Cell<char> bo = bsw.SwitchC();
            List<char> @out = new List<char>();
            using (bo.Listen(@out.Add))
            {
                esb.Send(new Sc(Maybe.Just('B'), Maybe.Just('b'), Maybe.Nothing<Cell<char>>()));
                esb.Send(new Sc(Maybe.Just('C'), Maybe.Just('c'), Maybe.Just(bb)));
                esb.Send(new Sc(Maybe.Just('D'), Maybe.Just('d'), Maybe.Nothing<Cell<char>>()));
                esb.Send(new Sc(Maybe.Just('E'), Maybe.Just('e'), Maybe.Just(ba)));
                esb.Send(new Sc(Maybe.Just('F'), Maybe.Just('f'), Maybe.Nothing<Cell<char>>()));
                esb.Send(new Sc(Maybe.Nothing<char>(), Maybe.Nothing<char>(), Maybe.Just(bb)));
                esb.Send(new Sc(Maybe.Nothing<char>(), Maybe.Nothing<char>(), Maybe.Just(ba)));
                esb.Send(new Sc(Maybe.Just('G'), Maybe.Just('g'), Maybe.Just(bb)));
                esb.Send(new Sc(Maybe.Just('H'), Maybe.Just('h'), Maybe.Just(ba)));
                esb.Send(new Sc(Maybe.Just('I'), Maybe.Just('i'), Maybe.Just(ba)));
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
            Sc2 sb1 = new Sc2(0);
            CellSink<Sc2> csb = new CellSink<Sc2>(sb1);
            Cell<int> bo = csb.Map<Cell<int>>(b => b.C).SwitchC();
            List<int> @out = new List<int>();
            using (bo.Listen(@out.Add))
            {
                Sc2 sc2 = new Sc2(3);
                Sc2 sb3 = new Sc2(4);
                Sc2 sb4 = new Sc2(7);
                sb1.C.Send(1);
                sb1.C.Send(2);
                csb.Send(sc2);
                sb1.C.Send(3);
                sc2.C.Send(4);
                sb3.C.Send(5);
                csb.Send(sb3);
                sb3.C.Send(6);
                sb3.C.Send(7);
                Transaction.RunVoid(() =>
                {
                    sb3.C.Send(2);
                    csb.Send(sb4);
                    sb4.C.Send(8);
                });
                sb4.C.Send(9);
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
            StreamSink<Ss> ess = new StreamSink<Ss>();
            // Split each field out of SB so we can update multiple behaviors in a
            // single transaction.
            Stream<char> sa = ess.Map(s => s.A);
            Stream<char> sb = ess.Map(s => s.B);
            Cell<Stream<char>> bsw = ess.Map(s => s.Sw).FilterMaybe().Hold(sa);
            Stream<char> so = bsw.SwitchS();
            List<char> @out = new List<char>();
            using (so.Listen(@out.Add))
            {
                ess.Send(new Ss('A', 'a', Maybe.Nothing<Stream<char>>()));
                ess.Send(new Ss('B', 'b', Maybe.Nothing<Stream<char>>()));
                ess.Send(new Ss('C', 'c', Maybe.Just(sb)));
                ess.Send(new Ss('D', 'd', Maybe.Nothing<Stream<char>>()));
                ess.Send(new Ss('E', 'e', Maybe.Just(sa)));
                ess.Send(new Ss('F', 'f', Maybe.Nothing<Stream<char>>()));
                ess.Send(new Ss('G', 'g', Maybe.Just(sb)));
                ess.Send(new Ss('H', 'h', Maybe.Just(sa)));
                ess.Send(new Ss('I', 'i', Maybe.Just(sa)));
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
                    cellSinks[7].Send(5);
                    cellSinks[14].Send(5);
                    cellSinks[23].Send(5);
                    cellSinks[35].Send(5);
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
                    cellSinks[7].Send(5);
                    cellSinks[14].Send(5);
                    cellSinks[23].Send(5);
                    cellSinks[35].Send(5);
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
            cellSinks[7].Send(5);
            Transaction.RunVoid(() =>
            {
                cellSinks[14].Send(5);
                cellSinks[23].Send(5);
                cellSinks[35].Send(5);
            });
            l.Unlisten();
            CollectionAssert.AreEqual(new[] { 58, 62, 74 }, @out);
        }
    }
}