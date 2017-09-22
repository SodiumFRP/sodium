using System;
using System.Collections.Generic;
using Sodium;

namespace Patterns
{
    internal class Program
    {
        private static void Main()
        {
            Dictionary<char, IExample> actions = new Dictionary<char, IExample>
            {
                {'a', new CalmExample()},
                {'b', new Pause()}
            };

            foreach (KeyValuePair<char, IExample> p in actions)
            {
                Console.WriteLine("(" + p.Key + ") " + p.Value.Name);
            }

            while (true)
            {
                Console.WriteLine();
                Console.Write("Select an example to run: ");
                ConsoleKeyInfo c = Console.ReadKey();
                IExample example;
                if (actions.TryGetValue(c.KeyChar, out example))
                {
                    Console.WriteLine();
                    Console.WriteLine();
                    example.Run();
                    break;
                }
                Console.WriteLine();
                Console.WriteLine("Invalid selection.");
            }
        }

        private interface IExample
        {
            string Name { get; }
            void Run();
        }

        private class CalmExample : IExample
        {
            public string Name { get; } = "Calm";

            public void Run()
            {
                DiscreteCellSink<int> sa = new DiscreteCellSink<int>(1);
                IListener l = Calm(sa).Listen(Console.WriteLine);
                sa.Send(1);
                sa.Send(2);
                sa.Send(2);
                sa.Send(4);
                sa.Send(4);
                sa.Send(1);
                l.Unlisten();
            }

            private static Stream<T> Calm<T>(Stream<T> sA, Lazy<IMaybe<T>> init)
            {
                return sA.CollectLazy(init, (a, lastA) =>
                {
                    IMaybe<T> ma = Maybe.Just(a);
                    return ma.Equals(lastA) ? (ReturnValue: Maybe.Nothing<T>(), State: lastA) : (ReturnValue: ma, State: ma);
                }).FilterMaybe();
            }

            private static Stream<T> Calm<T>(Stream<T> sA)
            {
                return Calm(sA, new Lazy<IMaybe<T>>(Maybe.Nothing<T>));
            }

            private static DiscreteCell<T> Calm<T>(DiscreteCell<T> a)
            {
                Lazy<T> initA = a.Cell.SampleLazy();
                Lazy<IMaybe<T>> mInitA = initA.Map(Maybe.Just);
                return Calm(a.Updates, mInitA).HoldLazy(initA);
            }
        }

        private class Pause : IExample
        {
            public string Name { get; } = "Pause";

            public void Run()
            {
                DiscreteCellSink<double> mainClock = new DiscreteCellSink<double>(0.0);
                StreamSink<Unit> sPause = new StreamSink<Unit>();
                StreamSink<Unit> sResume = new StreamSink<Unit>();
                DiscreteCell<double> gameClock = PausableClock(sPause, sResume, mainClock);
                IListener l = mainClock.Lift(gameClock, (m, g) => "main=" + m + " game=" + g).Listen(Console.WriteLine);
                mainClock.Send(1.0);
                mainClock.Send(2.0);
                mainClock.Send(3.0);
                sPause.Send(Unit.Value);
                mainClock.Send(4.0);
                mainClock.Send(5.0);
                mainClock.Send(6.0);
                sResume.Send(Unit.Value);
                mainClock.Send(7.0);
                l.Unlisten();
            }

            private static DiscreteCell<double> PausableClock(Stream<Unit> sPause, Stream<Unit> sResume, DiscreteCell<double> clock)
            {
                DiscreteCell<IMaybe<double>> pauseTime = sPause.Snapshot(clock, (_, t) => Maybe.Just(t)).OrElse(sResume.Map(_ => Maybe.Nothing<double>())).Hold(Maybe.Nothing<double>());
                DiscreteCell<double> lostTime = sResume.Accum(0.0, (_, total) =>
                {
                    double tPause = pauseTime.Cell.Sample().Match(v => v, () => 0);
                    double now = clock.Cell.Sample();
                    return total + (now - tPause);
                });
                return pauseTime.Lift(clock, lostTime, (otPause, tClk, tLost) => otPause.Match(v => v, () => tClk) - tLost);
            }
        }
    }
}