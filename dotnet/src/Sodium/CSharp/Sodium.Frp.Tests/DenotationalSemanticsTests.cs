using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Sodium.Frp.Tests
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
            (Stream<int> s, Dictionary<int, Action> sf) = MkStream(new Dictionary<int, int> { { 0, 5 }, { 1, 10 }, { 2, 12 } });
            List<int> @out = RunSimulation<int>(s.Map(x => x + 1).Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 6, 11, 13 }, @out);
        }

        [Test]
        public void Test_Snapshot_TestCase()
        {
            (Stream<char> s1, Dictionary<int, Action> s1F) = MkStream(new Dictionary<int, char> { { 0, 'a' }, { 3, 'b' }, { 5, 'c' } });
            (Stream<int> s2, Dictionary<int, Action> s2F) = MkStream(new Dictionary<int, int> { { 1, 4 }, { 5, 7 } });
            Cell<int> c = s2.Hold(3);
            List<int> @out = RunSimulation<int>(s1.Snapshot(c).Listen, new[] { s1F, s2F });
            CollectionAssert.AreEqual(new[] { 3, 4, 4 }, @out);
        }

        [Test]
        public void Test_Merge_TestCase()
        {
            (Stream<int> s1, Dictionary<int, Action> s1F) = MkStream(new Dictionary<int, int> { { 0, 0 }, { 2, 2 } });
            (Stream<int> s2, Dictionary<int, Action> s2F) = MkStream(new Dictionary<int, int> { { 1, 10 }, { 2, 20 }, { 3, 30 } });
            List<int> @out = RunSimulation<int>(s1.Merge(s2, (x, y) => x + y).Listen, new[] { s1F, s2F });
            CollectionAssert.AreEqual(new[] { 0, 10, 22, 30 }, @out);
        }

        [Test]
        public void Test_Filter_TestCase()
        {
            (Stream<int> s, Dictionary<int, Action> sf) = MkStream(new Dictionary<int, int> { { 0, 5 }, { 1, 6 }, { 2, 7 } });
            List<int> @out = RunSimulation<int>(s.Filter(x => x % 2 != 0).Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 5, 7 }, @out);
        }

        [Test]
        public void Test_SwitchS_TestCase()
        {
            RunPermutations<char>(
                createFiringsListAndListener =>
                {
                    (Stream<char> s1, Dictionary<int, Action> s1F) = MkStream(new Dictionary<int, char> { { 0, 'a' }, { 1, 'b' }, { 2, 'c' }, { 3, 'd' } });
                    (Stream<char> s2, Dictionary<int, Action> s2F) = MkStream(new Dictionary<int, char> { { 0, 'W' }, { 1, 'X' }, { 2, 'Y' }, { 3, 'Z' } });
                    (Stream<Stream<char>> switcher, Dictionary<int, Action> switcherF) = MkStream(new Dictionary<int, Stream<char>> { { 1, s2 } });
                    Cell<Stream<char>> c = switcher.Hold(s1);

                    IReadOnlyList<(string Name, Dictionary<int, Action> Firings)> firings = new[]
                    {
                        (Name: "s1", Firings: s1F),
                        (Name: "s2", Firings: s2F),
                        (Name: "switcher", Firings: switcherF)
                    };

                    return createFiringsListAndListener(firings, c.SwitchS().Listen);
                },
                @out => CollectionAssert.AreEqual(new[] { 'a', 'b', 'Y', 'Z' }, @out));
        }

        [Test]
        public void Test_Updates_TestCase()
        {
            (Stream<char> s, Dictionary<int, Action> sf) = MkStream(new Dictionary<int, char> { { 1, 'b' }, { 3, 'c' } });
            Cell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(c.Updates().Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 'b', 'c' }, @out);
        }

        [Test]
        public void Test_Value_TestCase1()
        {
            (Stream<char> s, Dictionary<int, Action> sf) = MkStream(new Dictionary<int, char> { { 1, 'b' }, { 3, 'c' } });
            Cell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(h => Transaction.Run(() => c.Values().Listen(h)), new[] { sf });
            CollectionAssert.AreEqual(new[] { 'a', 'b', 'c' }, @out);
        }

        [Test]
        public void Test_Value_TestCase2()
        {
            (Stream<char> s, Dictionary<int, Action> sf) = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 3, 'd' } });
            Cell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(h => Transaction.Run(() => c.Values().Listen(h)), new[] { sf });
            CollectionAssert.AreEqual(new[] { 'b', 'c', 'd' }, @out);
        }

        [Test]
        public void Test_ListenC_TestCase1()
        {
            (Stream<char> s, Dictionary<int, Action> sf) = MkStream(new Dictionary<int, char> { { 1, 'b' }, { 3, 'c' } });
            Cell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(c.Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 'a', 'b', 'c' }, @out);
        }

        [Test]
        public void Test_ListenC_TestCase2()
        {
            (Stream<char> s, Dictionary<int, Action> sf) = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 3, 'd' } });
            Cell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(c.Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 'b', 'c', 'd' }, @out);
        }

        [Test]
        public void Test_Split_TestCase()
        {
            (Stream<IReadOnlyList<char>> s, ILookup<int, Action> sf) = MkStream(
                new(int Time, IReadOnlyList<char> Value)[]
                {
                    (Time: 0, Value: new[] { 'a', 'b' }),
                    (Time: 1, Value: new[] { 'c' }),
                    (Time: 1, Value: new[] { 'd', 'e' })
                },
                (x, y) => x.Concat(y).ToArray());
            List<char> @out = RunSimulation<char>(Operational.Split<char, IReadOnlyList<char>>(s).Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 'a', 'b', 'c', 'd', 'e' }, @out);
        }

        [Test]
        public void Test_Constant_TestCase()
        {
            Cell<char> c = Cell.Constant('a');
            List<char> @out = RunSimulation<char>(c.Listen);
            CollectionAssert.AreEqual(new[] { 'a' }, @out);
        }

        [Test]
        public void Test_ConstantLazy_TestCase()
        {
            Cell<char> c = Cell.ConstantLazy(new Lazy<char>(() => 'a'));
            List<char> @out = RunSimulation<char>(c.Listen);
            CollectionAssert.AreEqual(new[] { 'a' }, @out);
        }

        [Test]
        public void Test_Hold_TestCase()
        {
            (Stream<char> s, Dictionary<int, Action> sf) = MkStream(new Dictionary<int, char> { { 1, 'b' }, { 3, 'c' } });
            Cell<char> c = s.Hold('a');
            List<char> @out = RunSimulation<char>(c.Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 'a', 'b', 'c' }, @out);
        }

        [Test]
        public void Test_MapC_TestCase()
        {
            (Stream<int> s, Dictionary<int, Action> sf) = MkStream(new Dictionary<int, int> { { 2, 3 }, { 3, 5 } });
            Cell<int> c = s.Hold(0);
            List<int> @out = RunSimulation<int>(c.Map(x => x + 1).Listen, new[] { sf });
            CollectionAssert.AreEqual(new[] { 1, 4, 6 }, @out);
        }

        [Test]
        public void Test_Apply_TestCase()
        {
            (Stream<int> s1, Dictionary<int, Action> s1F) = MkStream(new Dictionary<int, int> { { 1, 200 }, { 2, 300 }, { 4, 400 } });
            Cell<int> ca = s1.Hold(100);
            (Stream<Func<int, int>> s2, Dictionary<int, Action> s2F) = MkStream(new Dictionary<int, Func<int, int>> { { 1, x => x + 5 }, { 3, x => x + 6 } });
            Cell<Func<int, int>> cf = s2.Hold(x => x + 0);
            List<int> @out = RunSimulation<int>(ca.Apply(cf).Listen, new[] { s1F, s2F });
            CollectionAssert.AreEqual(new[] { 100, 205, 305, 306, 406 }, @out);
        }

        [Test]
        public void Test_SwitchC_TestCase1()
        {
            RunPermutations<char>(createFiringsListAndListener =>
                {
                    (Stream<char> s1, Dictionary<int, Action> s1F) = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 2, 'd' }, { 3, 'e' } });
                    Cell<char> c1 = s1.Hold('a');
                    (Stream<char> s2, Dictionary<int, Action> s2F) = MkStream(new Dictionary<int, char> { { 0, 'W' }, { 1, 'X' }, { 2, 'Y' }, { 3, 'Z' } });
                    Cell<char> c2 = s2.Hold('V');
                    (Stream<Cell<char>> switcher, Dictionary<int, Action> switcherF) = MkStream(new Dictionary<int, Cell<char>> { { 1, c2 } });
                    Cell<Cell<char>> c = switcher.Hold(c1);

                    IReadOnlyList<(string Name, Dictionary<int, Action> Firings)> firings = new[]
                    {
                        (Name: "s1", Firings: s1F),
                        (Name: "s2", Firings: s2F),
                        (Name: "switcher", Firings: switcherF)
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
                    (Stream<char> s1, Dictionary<int, Action> s1F) = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 2, 'd' }, { 3, 'e' } });
                    Cell<char> c1 = s1.Hold('a');
                    (Stream<char> s2, Dictionary<int, Action> s2F) = MkStream(new Dictionary<int, char> { { 1, 'X' }, { 2, 'Y' }, { 3, 'Z' } });
                    Cell<char> c2 = s2.Hold('W');
                    (Stream<Cell<char>> switcher, Dictionary<int, Action> switcherF) = MkStream(new Dictionary<int, Cell<char>> { { 1, c2 } });
                    Cell<Cell<char>> c = switcher.Hold(c1);

                    IReadOnlyList<(string Name, Dictionary<int, Action> Firings)> firings = new[]
                    {
                        (Name: "s1", Firings: s1F),
                        (Name: "s2", Firings: s2F),
                        (Name: "switcher", Firings: switcherF)
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
                    (Stream<char> s1, Dictionary<int, Action> s1F) = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 2, 'd' }, { 3, 'e' } });
                    Cell<char> c1 = s1.Hold('a');
                    (Stream<char> s2, Dictionary<int, Action> s2F) = MkStream(new Dictionary<int, char> { { 2, 'Y' }, { 3, 'Z' } });
                    Cell<char> c2 = s2.Hold('X');
                    (Stream<Cell<char>> switcher, Dictionary<int, Action> switcherF) = MkStream(new Dictionary<int, Cell<char>> { { 1, c2 } });
                    Cell<Cell<char>> c = switcher.Hold(c1);

                    IReadOnlyList<(string Name, Dictionary<int, Action> Firings)> firings = new[]
                    {
                        (Name: "s1", Firings: s1F),
                        (Name: "s2", Firings: s2F),
                        (Name: "switcher", Firings: switcherF)
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
                    (Stream<char> s1, Dictionary<int, Action> s1F) = MkStream(new Dictionary<int, char> { { 0, 'b' }, { 1, 'c' }, { 2, 'd' }, { 3, 'e' } });
                    Cell<char> c1 = s1.Hold('a');
                    (Stream<char> s2, Dictionary<int, Action> s2F) = MkStream(new Dictionary<int, char> { { 0, 'W' }, { 1, 'X' }, { 2, 'Y' }, { 3, 'Z' } });
                    Cell<char> c2 = s2.Hold('V');
                    (Stream<char> s3, Dictionary<int, Action> s3F) = MkStream(new Dictionary<int, char> { { 0, '2' }, { 1, '3' }, { 2, '4' }, { 3, '5' } });
                    Cell<char> c3 = s3.Hold('1');
                    (Stream<Cell<char>> switcher, Dictionary<int, Action> switcherF) = MkStream(new Dictionary<int, Cell<char>> { { 1, c2 }, { 3, c3 } });
                    Cell<Cell<char>> c = switcher.Hold(c1);

                    IReadOnlyList<(string Name, Dictionary<int, Action> Firings)> firings = new[]
                    {
                        (Name: "s1", Firings: s1F),
                        (Name: "s2", Firings: s2F),
                        (Name: "s3", Firings: s3F),
                        (Name: "switcher", Firings: switcherF)
                    };

                    return createFiringsListAndListener(firings, c.SwitchC().Listen);
                },
                @out => CollectionAssert.AreEqual(new[] { 'b', 'X', 'Y', '5' }, @out));
        }

        [Test]
        public void Test_Sample_TestCase()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            Cell<char> c = s.Hold('a');
            char sample1 = c.Sample();
            s.Send('b');
            char sample2 = c.Sample();
            Assert.AreEqual('a', sample1);
            Assert.AreEqual('b', sample2);
        }

        [Test]
        public void Test_SampleLazy_TestCase()
        {
            StreamSink<char> s = Stream.CreateSink<char>();
            Cell<char> c = s.Hold('a');
            Lazy<char> sample1 = c.SampleLazy();
            s.Send('b');
            Lazy<char> sample2 = c.SampleLazy();
            Assert.AreEqual('a', sample1.Value);
            Assert.AreEqual('b', sample2.Value);
        }

        private static (Stream<T> Stream, Dictionary<int, Action> Firings) MkStream<T>(Dictionary<int, T> firings)
        {
            StreamSink<T> s = Stream.CreateSink<T>();
            Dictionary<int, Action> f = firings.ToDictionary(firing => firing.Key, firing => (Action)(() => s.Send(firing.Value)));
            if (f.Keys.Any(k => k < 0))
            {
                throw new InvalidOperationException("All firings must occur at T >= 0.");
            }
            Stream<T> returnStream = s;
            return (Stream: returnStream, Firings: f);
        }

        private static (Stream<T> Stream, ILookup<int, Action> Firings) MkStream<T>(IReadOnlyList<(int Time, T Value)> firings, Func<T, T, T> coalesce)
        {
            StreamSink<T> s = Stream.CreateSink(coalesce);
            ILookup<int, Action> f = firings.ToLookup(firing => firing.Time, firing => (Action)(() => s.Send(firing.Value)));
            if (f.Any(g => g.Key < 0))
            {
                throw new InvalidOperationException("All firings must occur at T >= 0.");
            }
            Stream<T> returnStream = s;
            return (Stream: returnStream, Firings: f);
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
                void Run(int t)
                {
                    if (firings != null)
                    {
                        foreach (Action a in firings.SelectMany(f => f[t]))
                        {
                            a();
                        }
                    }
                }

                if (maxKey > -1)
                {
                    l = Transaction.Run(() =>
                    {
                        IListener lLocal = listen(@out.Add);
                        Run(0);
                        return lLocal;
                    });

                    for (int i = 1; i <= maxKey; i++)
                    {
                        int t = i;
                        Transaction.RunVoid(() => { Run(t); });
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

        private static void RunPermutations<T>(Func<Func<IReadOnlyList<(string Name, Dictionary<int, Action> Firings)>, Func<Action<T>, IListener>, (IReadOnlyList<(string Name, Dictionary<int, Action> Firings)> FiringsList, Func<Action<T>, IListener> Listen)>, (IReadOnlyList<(string Name, Dictionary<int, Action> Firings)> FiringsList, Func<Action<T>, IListener> Listen)> createListAndListener, Action<IReadOnlyList<T>> assert)
        {
            IReadOnlyList<int> indexes = Enumerable.Range(0, createListAndListener((fl, l) => (FiringsList: fl, Listen: l)).FiringsList.Count).ToArray();
            foreach ((IReadOnlyList<(string Name, Dictionary<int, Action> Firings)> firingsList, Func<Action<T>, IListener> listener) in
                GetPermutations(indexes).Select(ii =>
                {
                    (IReadOnlyList<(string Name, Dictionary<int, Action> Firings)> firingsList, Func<Action<T>, IListener> listen) = createListAndListener((fl, l) => (FiringsList: fl, Listen: l));
                    return (FiringsList: ii.Select(i => firingsList[i]).ToArray(), Listen: listen);
                }))
            {
                try
                {
                    List<T> @out = RunSimulation(listener, firingsList.Select(o => o.Firings).ToArray());
                    assert(@out);
                }
                catch
                {
                    Console.WriteLine("Test failed for ordering { " + string.Join(", ", firingsList.Select(o => o.Name)) + " }.");
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