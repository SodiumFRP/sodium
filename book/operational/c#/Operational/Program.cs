using System;
using System.Collections.Generic;
using Sodium;

namespace Operational
{
    internal static class Program
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
                if (actions.TryGetValue(c.KeyChar, out IExample example))
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
                DiscreteCellSink<int> x = new DiscreteCellSink<int>(0);
                IListener l = x.Listen(Console.WriteLine);
                x.Send(10);
                x.Send(20);
                x.Send(30);
                l.Unlisten();
            }
        }

        private class SameTransaction : IExample
        {
            public string Name { get; } = "Same Transaction";

            public void Run()
            {
                StreamSink<int> sX = new StreamSink<int>();
                Stream<int> sXPlus1 = sX.Map(x => x + 1);
                IListener l = Transaction.Run(() =>
                {
                    sX.Send(1);
                    return sXPlus1.Listen(Console.WriteLine);
                });
                sX.Send(2);
                sX.Send(3);
                l.Unlisten();
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
                IListener l = new CompositeListener(new[]
                {
                    sX.Listen(x => sY.Send(x)),
                    sY.Listen(Console.WriteLine)
                });
                sX.Send(1);
                sX.Send(2);
                sX.Send(3);
                l.Unlisten();
            }
        }

        private class Split : IExample
        {
            public string Name { get; } = "Split";

            public void Run()
            {
                StreamSink<IReadOnlyList<int>> @as = new StreamSink<IReadOnlyList<int>>();
                IListener l = Sodium.Operational.Split<int, IReadOnlyList<int>>(@as).Accum(0, (a, b) => a + b).Updates.Listen(Console.WriteLine);
                @as.Send(new[] { 100, 15, 60 });
                @as.Send(new[] { 1, 5 });
                l.Unlisten();
            }
        }

        private class Stream : IExample
        {
            public string Name { get; } = "Stream";

            public void Run()
            {
                StreamSink<int> sX = new StreamSink<int>();
                Stream<int> sXPlus1 = sX.Map(x => x + 1);
                IListener l = sXPlus1.Listen(Console.WriteLine);
                sX.Send(1);
                sX.Send(2);
                sX.Send(3);
                l.Unlisten();
            }
        }

        private class Updates : IExample
        {
            public string Name { get; } = "Updates";

            public void Run()
            {
                DiscreteCellSink<int> x = new DiscreteCellSink<int>(0);
                x.Send(1);
                IListener l = x.Updates.Listen(Console.WriteLine);
                x.Send(2);
                x.Send(3);
                l.Unlisten();
            }
        }

        private class Value1 : IExample
        {
            public string Name { get; } = "Value 1";

            public void Run()
            {
                DiscreteCellSink<int> x = new DiscreteCellSink<int>(0);
                x.Send(1);
                IListener l = x.Values.Listen(Console.WriteLine);
                x.Send(2);
                x.Send(3);
                l.Unlisten();
            }
        }

        private class Value2 : IExample
        {
            public string Name { get; } = "Value 2";

            public void Run()
            {
                CellSink<int> x = new CellSink<int>(0);
                x.Send(1);
                IListener l = Transaction.Run(() => Sodium.Operational.Value(x).Listen(Console.WriteLine));
                x.Send(2);
                x.Send(3);
                l.Unlisten();
            }
        }
    }
}