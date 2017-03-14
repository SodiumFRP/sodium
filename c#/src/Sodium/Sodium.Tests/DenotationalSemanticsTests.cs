using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class DenotationalSemanticsTests
    {
        [Test]
        public void Test_Never_TestCase()
        {
            List<int> @out = RunSimulation<int>(Stream.Never<int>().Listen);
            CollectionAssert.AreEqual(new int[0], @out);
        }

        [Test]
        public void Test_MapS_TestCase()
        {
            ValueTuple<Stream<int>, Dictionary<int, Action>> st = MkStream(new Dictionary<int, int> { { 0, 5 }, { 1, 10 }, { 2, 12 } });
            Stream<int> s = st.Item1;
            Dictionary<int, Action> sf = st.Item2;
            List<int> @out = RunSimulation<int>(s.Map(x => x + 1).Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 6, 11, 13 }, @out);
        }

        [Test]
        public void Test_Snapshot_TestCase()
        {
            ValueTuple<Stream<char>, Dictionary<int, Action>> s1T = MkStream(new Dictionary<int, char> { { 0, 'a' }, { 3, 'b' }, { 5, 'c' } });
            Stream<char> s1 = s1T.Item1;
            Dictionary<int, Action> s1F = s1T.Item2;
            ValueTuple<Stream<int>, Dictionary<int, Action>> s2T = MkStream(new Dictionary<int, int> { { 1, 4 }, { 5, 7 } });
            Stream<int> s2 = s2T.Item1;
            Dictionary<int, Action> s2F = s2T.Item2;
            DiscreteCell<int> c = s2.Hold(3);
            List<int> @out = RunSimulation<int>(s1.Snapshot(c.Cell).Listen, new[] { s1F, s2F });
            CollectionAssert.AreEqual(new[] { 3, 4, 4 }, @out);
        }

        [Test]
        public void Test_Merge_TestCase()
        {
            ValueTuple<Stream<int>, Dictionary<int, Action>> s1T = MkStream(new Dictionary<int, int> { { 0, 0 }, { 2, 2 } });
            Stream<int> s1 = s1T.Item1;
            Dictionary<int, Action> s1F = s1T.Item2;
            ValueTuple<Stream<int>, Dictionary<int, Action>> s2T = MkStream(new Dictionary<int, int> { { 1, 10 }, { 2, 20 }, { 3, 30 } });
            Stream<int> s2 = s2T.Item1;
            Dictionary<int, Action> s2F = s2T.Item2;
            List<int> @out = RunSimulation<int>(s1.Merge(s2, (x, y) => x + y).Listen, new[] { s1F, s2F });
            CollectionAssert.AreEqual(new[] { 0, 10, 22, 30 }, @out);
        }

        [Test]
        public void Test_Filter_TestCase()
        {
            ValueTuple<Stream<int>, Dictionary<int, Action>> st = MkStream(new Dictionary<int, int> { { 0, 5 }, { 1, 6 }, { 2, 7 } });
            Stream<int> s = st.Item1;
            Dictionary<int, Action> sf = st.Item2;
            List<int> @out = RunSimulation<int>(s.Filter(x => x % 2 != 0).Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 5, 7 }, @out);
        }

        [Test]
        public void Test_SwitchS_TestCase()
        {
            RunPermutations<char>(
                createFiringsListAndListener =>
                {
                    ValueTuple<Stream<char>, Dictionary<int, Action>> s1T = MkStream(new Dictionary<int, char> { { 0, 'a' }, { 1, 'b' }, { 2, 'c' }, { 3, 'd' } });
                    Stream<char> s1 = s1T.Item1;
                    Dictionary<int, Action> s1F = s1T.Item2;
                    ValueTuple<Stream<char>, Dictionary<int, Action>> s2T = MkStream(new Dictionary<int, char> { { 0, 'W' }, { 1, 'X' }, { 2, 'Y' }, { 3, 'Z' } });
                    Stream<char> s2 = s2T.Item1;
                    Dictionary<int, Action> s2F = s2T.Item2;
                    ValueTuple<Stream<Stream<char>>, Dictionary<int, Action>> switcherT = MkStream(new Dictionary<int, Stream<char>> { { 1, s2 } });
                    Stream<Stream<char>> switcher = switcherT.Item1;
                    Dictionary<int, Action> switcherF = switcherT.Item2;
                    DiscreteCell<Stream<char>> c = switcher.Hold(s1);

                    IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>> firings = new[]
                    {
                        ValueTuple.Create("s1", s1F),
                        ValueTuple.Create("s2", s2F),
                        ValueTuple.Create("switcher", switcherF)
                    };

                    return createFiringsListAndListener(firings, c.Cell.SwitchS().Listen);
                },
                @out => CollectionAssert.AreEqual(new[] { 'a', 'b', 'Y', 'Z' }, @out));
        }

        [Test]
        public void Test_Updates_TestCase()
        {
            ValueTuple<Stream<char>, Dictionary<int, Action>> st = MkStream(new Dictionary<int, char> { { 1, 'b' }, { 3, 'c' } });
            Stream<char> s = st.Item1;
            Dictionary<int, Action> sf = st.Item2;
            DiscreteCell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(Operational.Updates(c.Cell).Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 'b', 'c' }, @out);
        }

        [Test]
        public void Test_Value_TestCase1()
        {
            ValueTuple<Stream<char>, Dictionary<int, Action>> st = MkStream(new Dictionary<int, char> { { 1, 'b' }, { 3, 'c' } });
            Stream<char> s = st.Item1;
            Dictionary<int, Action> sf = st.Item2;
            DiscreteCell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(h => Transaction.Run(() => Operational.Value(c.Cell).Listen(h)), new[] { sf });
            CollectionAssert.AreEqual(new[] { 'a', 'b', 'c' }, @out);
        }

        [Test]
        public void Test_Value_TestCase2()
        {
            ValueTuple<Stream<char>, Dictionary<int, Action>> st = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 3, 'd' } });
            Stream<char> s = st.Item1;
            Dictionary<int, Action> sf = st.Item2;
            DiscreteCell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(h => Transaction.Run(() => Operational.Value(c.Cell).Listen(h)), new[] { sf });
            CollectionAssert.AreEqual(new[] { 'b', 'c', 'd' }, @out);
        }

        [Test]
        public void Test_ListenC_TestCase1()
        {
            ValueTuple<Stream<char>, Dictionary<int, Action>> st = MkStream(new Dictionary<int, char> { { 1, 'b' }, { 3, 'c' } });
            Stream<char> s = st.Item1;
            Dictionary<int, Action> sf = st.Item2;
            DiscreteCell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(c.Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 'a', 'b', 'c' }, @out);
        }

        [Test]
        public void Test_ListenC_TestCase2()
        {
            ValueTuple<Stream<char>, Dictionary<int, Action>> st = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 3, 'd' } });
            Stream<char> s = st.Item1;
            Dictionary<int, Action> sf = st.Item2;
            DiscreteCell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(c.Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 'b', 'c', 'd' }, @out);
        }

        [Test]
        public void Test_Split_TestCase()
        {
            ValueTuple<Stream<IReadOnlyList<char>>, ILookup<int, Action>> st = MkStream(new[]
            {
                ValueTuple.Create<int, IReadOnlyList<char>>(0, new[] {'a', 'b'}),
                ValueTuple.Create<int, IReadOnlyList<char>>(1, new[] {'c'}),
                ValueTuple.Create<int, IReadOnlyList<char>>(1, new[] {'d', 'e'})
            },
                (x, y) => x.Concat(y).ToArray());
            Stream<IReadOnlyList<char>> s = st.Item1;
            ILookup<int, Action> sf = st.Item2;
            List<char> @out = RunSimulation<char>(Operational.Split<char, IReadOnlyList<char>>(s).Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 'a', 'b', 'c', 'd', 'e' }, @out);
        }

        [Test]
        public void Test_Constant_TestCase()
        {
            DiscreteCell<char> c = DiscreteCell.Constant('a');
            List<char> @out = RunSimulation<char>(c.Listen);
            CollectionAssert.AreEqual(new[] { 'a' }, @out);
        }

        [Test]
        public void Test_ConstantLazy_TestCase()
        {
            DiscreteCell<char> c = DiscreteCell.ConstantLazy(new Lazy<char>(() => 'a'));
            List<char> @out = RunSimulation<char>(c.Listen);
            CollectionAssert.AreEqual(new[] { 'a' }, @out);
        }

        [Test]
        public void Test_Hold_TestCase()
        {
            ValueTuple<Stream<char>, Dictionary<int, Action>> st = MkStream(new Dictionary<int, char> { { 1, 'b' }, { 3, 'c' } });
            Stream<char> s = st.Item1;
            Dictionary<int, Action> sf = st.Item2;
            DiscreteCell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(c.Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 'a', 'b', 'c' }, @out);
        }

        [Test]
        public void Test_MapC_TestCase()
        {
            ValueTuple<Stream<int>, Dictionary<int, Action>> st = MkStream(new Dictionary<int, int> { { 2, 3 }, { 3, 5 } });
            Stream<int> s = st.Item1;
            Dictionary<int, Action> sf = st.Item2;
            DiscreteCell<int> c = s.Hold(0);
            List<int> @out = RunSimulation<int>(c.Map(x => x + 1).Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 1, 4, 6 }, @out);
        }

        [Test]
        public void Test_Apply_TestCase()
        {
            ValueTuple<Stream<int>, Dictionary<int, Action>> s1T = MkStream(new Dictionary<int, int> { { 1, 200 }, { 2, 300 }, { 4, 400 } });
            Stream<int> s1 = s1T.Item1;
            Dictionary<int, Action> s1F = s1T.Item2;
            DiscreteCell<int> ca = s1.Hold(100);
            ValueTuple<Stream<Func<int, int>>, Dictionary<int, Action>> s2T = MkStream(new Dictionary<int, Func<int, int>> { { 1, x => x + 5 }, { 3, x => x + 6 } });
            Stream<Func<int, int>> s2 = s2T.Item1;
            Dictionary<int, Action> s2F = s2T.Item2;
            DiscreteCell<Func<int, int>> cf = s2.Hold(x => x + 0);
            List<int> @out = RunSimulation<int>(ca.Apply(cf).Listen, new[] { s1F, s2F });
            CollectionAssert.AreEqual(new[] { 100, 205, 305, 306, 406 }, @out);
        }

        [Test]
        public void Test_SwitchC_TestCase1()
        {
            RunPermutations<char>(createFiringsListAndListener =>
            {
                ValueTuple<Stream<char>, Dictionary<int, Action>> s1T = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 2, 'd' }, { 3, 'e' } });
                Stream<char> s1 = s1T.Item1;
                Dictionary<int, Action> s1F = s1T.Item2;
                DiscreteCell<char> c1 = s1.Hold('a');
                ValueTuple<Stream<char>, Dictionary<int, Action>> s2T = MkStream(new Dictionary<int, char> { { 0, 'W' }, { 1, 'X' }, { 2, 'Y' }, { 3, 'Z' } });
                Stream<char> s2 = s2T.Item1;
                Dictionary<int, Action> s2F = s2T.Item2;
                DiscreteCell<char> c2 = s2.Hold('V');
                ValueTuple<Stream<DiscreteCell<char>>, Dictionary<int, Action>> switcherT = MkStream(new Dictionary<int, DiscreteCell<char>> { { 1, c2 } });
                Stream<DiscreteCell<char>> switcher = switcherT.Item1;
                Dictionary<int, Action> switcherF = switcherT.Item2;
                DiscreteCell<DiscreteCell<char>> c = switcher.Hold(c1);

                IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>> firings = new[]
                {
                    ValueTuple.Create("s1", s1F),
                    ValueTuple.Create("s2", s2F),
                    ValueTuple.Create("switcher", switcherF)
                };

                return createFiringsListAndListener(firings, c.SwitchC().Listen);
            },
                @out => CollectionAssert.AreEqual(new[] { 'b', 'X', 'Y', 'Z' }, @out));
        }

        [Test]
        public void Test_SwitchC_TestCase2()
        {
            RunPermutations<char>(createFiringsListAndListener =>
            {
                ValueTuple<Stream<char>, Dictionary<int, Action>> s1T = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 2, 'd' }, { 3, 'e' } });
                Stream<char> s1 = s1T.Item1;
                Dictionary<int, Action> s1F = s1T.Item2;
                DiscreteCell<char> c1 = s1.Hold('a');
                ValueTuple<Stream<char>, Dictionary<int, Action>> s2T = MkStream(new Dictionary<int, char> { { 1, 'X' }, { 2, 'Y' }, { 3, 'Z' } });
                Stream<char> s2 = s2T.Item1;
                Dictionary<int, Action> s2F = s2T.Item2;
                DiscreteCell<char> c2 = s2.Hold('W');
                ValueTuple<Stream<DiscreteCell<char>>, Dictionary<int, Action>> switcherT = MkStream(new Dictionary<int, DiscreteCell<char>> { { 1, c2 } });
                Stream<DiscreteCell<char>> switcher = switcherT.Item1;
                Dictionary<int, Action> switcherF = switcherT.Item2;
                DiscreteCell<DiscreteCell<char>> c = switcher.Hold(c1);

                IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>> firings = new[]
                {
                    ValueTuple.Create("s1", s1F),
                    ValueTuple.Create("s2", s2F),
                    ValueTuple.Create("switcher", switcherF)
                };

                return createFiringsListAndListener(firings, c.SwitchC().Listen);
            },
                @out => CollectionAssert.AreEqual(new[] { 'b', 'X', 'Y', 'Z' }, @out));
        }

        [Test]
        public void Test_SwitchC_TestCase3()
        {
            RunPermutations<char>(createFiringsListAndListener =>
            {
                ValueTuple<Stream<char>, Dictionary<int, Action>> s1T = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 2, 'd' }, { 3, 'e' } });
                Stream<char> s1 = s1T.Item1;
                Dictionary<int, Action> s1F = s1T.Item2;
                DiscreteCell<char> c1 = s1.Hold('a');
                ValueTuple<Stream<char>, Dictionary<int, Action>> s2T = MkStream(new Dictionary<int, char> { { 2, 'Y' }, { 3, 'Z' } });
                Stream<char> s2 = s2T.Item1;
                Dictionary<int, Action> s2F = s2T.Item2;
                DiscreteCell<char> c2 = s2.Hold('X');
                ValueTuple<Stream<DiscreteCell<char>>, Dictionary<int, Action>> switcherT = MkStream(new Dictionary<int, DiscreteCell<char>> { { 1, c2 } });
                Stream<DiscreteCell<char>> switcher = switcherT.Item1;
                Dictionary<int, Action> switcherF = switcherT.Item2;
                DiscreteCell<DiscreteCell<char>> c = switcher.Hold(c1);

                IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>> firings = new[]
                {
                    ValueTuple.Create("s1", s1F),
                    ValueTuple.Create("s2", s2F),
                    ValueTuple.Create("switcher", switcherF)
                };

                return createFiringsListAndListener(firings, c.SwitchC().Listen);
            },
                @out => CollectionAssert.AreEqual(new[] { 'b', 'X', 'Y', 'Z' }, @out));
        }

        [Test]
        public void Test_SwitchC_TestCase4()
        {
            RunPermutations<char>(createFiringsListAndListener =>
            {
                ValueTuple<Stream<char>, Dictionary<int, Action>> s1T = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 2, 'd' }, { 3, 'e' } });
                Stream<char> s1 = s1T.Item1;
                Dictionary<int, Action> s1F = s1T.Item2;
                DiscreteCell<char> c1 = s1.Hold('a');
                ValueTuple<Stream<char>, Dictionary<int, Action>> s2T = MkStream(new Dictionary<int, char> { { 0, 'W' }, { 1, 'X' }, { 2, 'Y' }, { 3, 'Z' } });
                Stream<char> s2 = s2T.Item1;
                Dictionary<int, Action> s2F = s2T.Item2;
                DiscreteCell<char> c2 = s2.Hold('V');
                ValueTuple<Stream<char>, Dictionary<int, Action>> s3T = MkStream(new Dictionary<int, char> { { 0, '2' }, { 1, '3' }, { 2, '4' }, { 3, '5' } });
                Stream<char> s3 = s3T.Item1;
                Dictionary<int, Action> s3F = s3T.Item2;
                DiscreteCell<char> c3 = s3.Hold('1');
                ValueTuple<Stream<DiscreteCell<char>>, Dictionary<int, Action>> switcherT = MkStream(new Dictionary<int, DiscreteCell<char>> { { 1, c2 }, { 3, c3 } });
                Stream<DiscreteCell<char>> switcher = switcherT.Item1;
                Dictionary<int, Action> switcherF = switcherT.Item2;
                DiscreteCell<DiscreteCell<char>> c = switcher.Hold(c1);

                IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>> firings = new[]
                {
                    ValueTuple.Create("s1", s1F),
                    ValueTuple.Create("s2", s2F),
                    ValueTuple.Create("s3", s3F),
                    ValueTuple.Create("switcher", switcherF)
                };

                return createFiringsListAndListener(firings, c.SwitchC().Listen);
            },
                @out => CollectionAssert.AreEqual(new[] { 'b', 'X', 'Y', '5' }, @out));
        }

        [Test]
        public void Test_Sample_TestCase()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            DiscreteCell<char> c = s.Hold('a');
            char sample1 = c.Cell.Sample();
            s.Send('b');
            char sample2 = c.Cell.Sample();
            Assert.AreEqual('a', sample1);
            Assert.AreEqual('b', sample2);
        }

        [Test]
        public void Test_SampleLazy_TestCase()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            DiscreteCell<char> c = s.Hold('a');
            Lazy<char> sample1 = c.Cell.SampleLazy();
            s.Send('b');
            Lazy<char> sample2 = c.Cell.SampleLazy();
            Assert.AreEqual('a', sample1.Value);
            Assert.AreEqual('b', sample2.Value);
        }

        private static ValueTuple<Stream<T>, Dictionary<int, Action>> MkStream<T>(Dictionary<int, T> firings)
        {
            StreamSink<T> s = Stream.CreateSink<T>();
            Dictionary<int, Action> f = firings.ToDictionary(firing => firing.Key, firing => (Action)(() => s.Send(firing.Value)));
            if (f.Keys.Any(k => k < 0))
            {
                throw new InvalidOperationException("All firings must occur at T >= 0.");
            }
            Stream<T> returnStream = s;
            return ValueTuple.Create(returnStream, f);
        }

        private static ValueTuple<Stream<T>, ILookup<int, Action>> MkStream<T>(IReadOnlyList<ValueTuple<int, T>> firings, Func<T, T, T> coalesce)
        {
            StreamSink<T> s = Stream.CreateSink<T>(coalesce);
            ILookup<int, Action> f = firings.ToLookup(firing => firing.Item1, firing => (Action)(() => s.Send(firing.Item2)));
            if (f.Any(g => g.Key < 0))
            {
                throw new InvalidOperationException("All firings must occur at T >= 0.");
            }
            Stream<T> returnStream = s;
            return ValueTuple.Create(returnStream, f);
        }

        private static List<T> RunSimulation<T>(Func<Action<T>, IListener> listen, IReadOnlyList<Dictionary<int, Action>> firings)
        {
            return RunSimulation(listen, firings.Select(f => f.ToLookup(p => p.Key, p => p.Value)).ToArray());
        }

        private static List<T> RunSimulation<T>(Func<Action<T>, IListener> listen, IReadOnlyList<ILookup<int, Action>> firings = null)
        {
            int maxKey = firings?.SelectMany(d => d.Select(g => g.Key)).DefaultIfEmpty(-1).Max() ?? -1;
            List<T> @out = new List<T>();
            IListener l = null;
            try
            {
                Action<int> run = t =>
                {
                    if (firings != null)
                    {
                        foreach (Action a in firings.SelectMany(f => f[t]))
                        {
                            a();
                        }
                    }
                };

                if (maxKey > -1)
                {
                    l = Transaction.Run(() =>
                    {
                        IListener lLocal = listen(@out.Add);
                        run(0);
                        return lLocal;
                    });

                    for (int i = 1; i <= maxKey; i++)
                    {
                        int t = i;
                        Transaction.RunVoid(() => { run(t); });
                    }
                }
                else
                {
                    l = listen(@out.Add);
                }
            }
            finally
            {
                l?.Unlisten();
            }
            return @out;
        }

        private static void RunPermutations<T>(Func<Func<IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>>, Func<Action<T>, IListener>, ValueTuple<IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>>, Func<Action<T>, IListener>>>, ValueTuple<IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>>, Func<Action<T>, IListener>>> createListAndListener, Action<IReadOnlyList<T>> assert)
        {
            IReadOnlyList<int> indexes = Enumerable.Range(0, createListAndListener(ValueTuple.Create).Item1.Count).ToArray();
            foreach (ValueTuple<IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>>, Func<Action<T>, IListener>> listAndListener in GetPermutations(indexes).Select(ii =>
            {
                ValueTuple<IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>>, Func<Action<T>, IListener>> listAndListenerLocal = createListAndListener(ValueTuple.Create);
                IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>> l = listAndListenerLocal.Item1;
                return ValueTuple.Create<IReadOnlyList<ValueTuple<string, Dictionary<int, Action>>>, Func<Action<T>, IListener>>(ii.Select(i => l[i]).ToArray(), listAndListenerLocal.Item2);
            }))
            {
                try
                {
                    List<T> @out = RunSimulation(listAndListener.Item2, listAndListener.Item1.Select(o => o.Item2).ToArray());
                    assert(@out);
                }
                catch
                {
                    Console.WriteLine("Test failed for ordering { " + string.Join(", ", listAndListener.Item1.Select(o => o.Item1)) + " }.");
                    throw;
                }
            }
        }

        private static IReadOnlyList<IReadOnlyList<T>> GetPermutations<T>(IReadOnlyList<T> list)
        {
            return GetPermutations(list, list.Count);
        }

        private static IReadOnlyList<IReadOnlyList<T>> GetPermutations<T>(IReadOnlyList<T> list, int length)
        {
            if (length == 1)
            {
                return list.Select(t => new[] { t }).ToArray();
            }

            return GetPermutations(list, length - 1).SelectMany(t => list.Where(e => !t.Contains(e)), (t1, t2) => t1.Concat(new[] { t2 }).ToArray()).ToArray();
        }
    }
}