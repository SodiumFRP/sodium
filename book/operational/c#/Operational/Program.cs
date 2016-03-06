using System;
using System.Collections.Generic;
using Sodium;

namespace Operational
{
    internal class Program
    {
        private static void Main()
        {
            Dictionary<char, IExample> actions = new Dictionary<char, IExample>
            {
                { 'a', new Cell() },
                { 'b', new SameTransaction() },
                { 'c', new SendInCallback() },
                { 'd', new Split() },
                { 'e', new Stream() },
                { 'f', new Updates() },
                { 'g', new Value1() },
                { 'h', new Value2() }
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

        private class Cell : IExample
        {
            public string Name { get; } = "Cell";

            public void Run()
            {
                CellSink<int> x = new CellSink<int>(0);
                using (x.Listen(Console.WriteLine))
                {
                    x.Send(10);
                    x.Send(20);
                    x.Send(30);
                }
            }
        }

        private class SameTransaction : IExample
        {
            public string Name { get; } = "Same Transaction";

            public void Run()
            {
                StreamSink<int> sX = new StreamSink<int>();
                Stream<int> sXPlus1 = sX.Map(x => x + 1);
                using (Transaction.Run(() =>
                {
                    sX.Send(1);
                    return sXPlus1.Listen(Console.WriteLine);
                }))
                {
                    sX.Send(2);
                    sX.Send(3);
                }
            }
        }

        private class SendInCallback : IExample
        {
            public string Name { get; } = "Send In Callback";

            public void Run()
            {
                StreamSink<int> sX = new StreamSink<int>();
                StreamSink<int> sY = new StreamSink<int>();
                // Should throw an exception because you're not allowed to use Send() inside
                // a callback.
                using (new ImmutableCompositeListener(new[]
                {
                    sX.Listen(x => sY.Send(x)),
                    sY.Listen(Console.WriteLine)
                }))
                {
                    sX.Send(1);
                    sX.Send(2);
                    sX.Send(3);
                }
            }
        }

        private class Split : IExample
        {
            public string Name { get; } = "Split";

            public void Run()
            {
                StreamSink<IReadOnlyList<int>> @as = new StreamSink<IReadOnlyList<int>>();
                using (Sodium.Operational.Updates(Sodium.Operational.Split<int, IReadOnlyList<int>>(@as).Accum(0, (a, b) => a + b)).Listen(Console.WriteLine))
                {
                    @as.Send(new[] { 100, 15, 60 });
                    @as.Send(new[] { 1, 5 });
                }
            }
        }

        private class Stream : IExample
        {
            public string Name { get; } = "Stream";

            public void Run()
            {
                StreamSink<int> sX = new StreamSink<int>();
                Stream<int> sXPlus1 = sX.Map(x => x + 1);
                using (sXPlus1.Listen(Console.WriteLine))
                {
                    sX.Send(1);
                    sX.Send(2);
                    sX.Send(3);
                }
            }
        }

        private class Updates : IExample
        {
            public string Name { get; } = "Updates";

            public void Run()
            {
                CellSink<int> x = new CellSink<int>(0);
                x.Send(1);
                using (Sodium.Operational.Updates(x).Listen(Console.WriteLine))
                {
                    x.Send(2);
                    x.Send(3);
                }
            }
        }

        private class Value1 : IExample
        {
            public string Name { get; } = "Value 1";

            public void Run()
            {
                CellSink<int> x = new CellSink<int>(0);
                x.Send(1);
                using (Sodium.Operational.Value(x).Listen(Console.WriteLine))
                {
                    x.Send(2);
                    x.Send(3);
                }
            }
        }

        private class Value2 : IExample
        {
            public string Name { get; } = "Value 2";

            public void Run()
            {
                CellSink<int> x = new CellSink<int>(0);
                x.Send(1);
                using (Transaction.Run(() => Sodium.Operational.Value(x).Listen(Console.WriteLine)))
                {
                    x.Send(2);
                    x.Send(3);
                }
            }
        }
    }
}