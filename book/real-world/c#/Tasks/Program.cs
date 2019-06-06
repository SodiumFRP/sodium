using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Sodium.Frp;

namespace Tasks
{
    internal class Program
    {
        private static void Main()
        {
            MainAsync().GetAwaiter().GetResult();
        }

        private static async Task MainAsync()
        {
            Dictionary<char, IExample> actions = new Dictionary<char, IExample>
            {
                {'a', new Promise1()},
                {'b', new Promise1WithThreads()},
                {'c', new Promise2()},
                {'d', new Promise2WithThreads()}
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
                    await example.Run();
                    break;
                }
                Console.WriteLine();
                Console.WriteLine("Invalid selection.");
            }
        }

        private interface IExample
        {
            string Name { get; }
            Task Run();
        }

        private class Promise1 : IExample
        {
            public string Name { get; } = "Promise 1";

            public async Task Run()
            {
                Console.WriteLine("*** test 1");
                {
                    StreamSink<string> s1 = Stream.CreateSink<string>();
                    TaskWithListener t1 = s1.ListenOnceAsync(t => t.ContinueWith(t2 => Console.WriteLine(t2.Result), TaskContinuationOptions.ExecuteSynchronously));
                    s1.Send("Early");
                    await Task.Delay(500);
                    Console.WriteLine("Before await");
                    await t1;
                    Console.WriteLine("After await");
                }

                Console.WriteLine();

                Console.WriteLine("*** test 2");
                {
                    StreamSink<string> s1 = Stream.CreateSink<string>();
                    TaskWithListener<string> t1 = s1.ListenOnceAsync();
                    s1.Send("Late");
                    await Task.Delay(500);
                    Console.WriteLine("Before await");
                    Console.WriteLine(await t1);
                    Console.WriteLine("After await");
                }
            }
        }

        private class Promise1WithThreads : IExample
        {
            public string Name { get; } = "Promise 1 With Threads";

            public async Task Run()
            {
                Console.WriteLine("*** test");
                {
                    StreamSink<string> s1 = Stream.CreateSink<string>();
                    TaskWithListener<string> t1 = s1.ListenOnceAsync();

                    new Thread(() =>
                    {
                        s1.Send("Sent");
                    }).Start();

                    Console.WriteLine(await t1);
                }
            }
        }

        private class Promise2 : IExample
        {
            public string Name { get; } = "Promise 2";

            public async Task Run()
            {
                Console.WriteLine("*** Simple test");
                {
                    StreamSink<string> sa = Stream.CreateSink<string>();
                    StreamSink<string> sb = Stream.CreateSink<string>();
                    TaskWithListener<string> ta = sa.ListenOnceAsync();
                    TaskWithListener<string> tb = sb.ListenOnceAsync();
                    sa.Send("Hello");
                    sb.Send("World");
                    Console.WriteLine(await ta + " " + await tb);
                }

                Console.WriteLine();

                Console.WriteLine("*** Simultaneous case");
                {
                    StreamSink<string> sa = Stream.CreateSink<string>();
                    StreamSink<string> sb = Stream.CreateSink<string>();
                    TaskWithListener<string> ta = sa.ListenOnceAsync();
                    TaskWithListener<string> tb = sb.ListenOnceAsync();
                    Transaction.RunVoid(() =>
                    {
                        sa.Send("Hello");
                        sb.Send("World");
                    });
                    Console.WriteLine(await ta + " " + await tb);
                }
            }
        }

        private class Promise2WithThreads : IExample
        {
            public string Name { get; } = "Promise 2 With Threads";

            public async Task Run()
            {
                Console.WriteLine("*** Simple test");
                {
                    StreamSink<string> sa = Stream.CreateSink<string>();
                    StreamSink<string> sb = Stream.CreateSink<string>();
                    TaskWithListener<string> ta = sa.ListenOnceAsync();
                    TaskWithListener<string> tb = sb.ListenOnceAsync();

                    new Thread(() =>
                    {
                        sa.Send("Hello");
                        sb.Send("World");
                    }).Start();

                    Console.WriteLine(await ta + " " + await tb);
                }

                Console.WriteLine();

                Console.WriteLine("*** Simultaneous case");
                {
                    StreamSink<string> sa = Stream.CreateSink<string>();
                    StreamSink<string> sb = Stream.CreateSink<string>();
                    TaskWithListener<string> ta = sa.ListenOnceAsync();
                    TaskWithListener<string> tb = sb.ListenOnceAsync();

                    new Thread(() =>
                    {
                        Transaction.RunVoid(() =>
                        {
                            sa.Send("Hello");
                            sb.Send("World");
                        });
                    }).Start();

                    Console.WriteLine(await ta + " " + await tb);
                }
            }
        }
    }
}