using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Sodium;

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
                    StreamSink<string> s1 = new StreamSink<string>();
                    Task<string> t1 = s1.ListenOnce();
                    s1.Send("Early");
                    Console.WriteLine(await t1);
                }

                Console.WriteLine();

                Console.WriteLine("*** test 2");
                {
                    StreamSink<string> s1 = new StreamSink<string>();
                    Task t1 = s1.ListenOnce().ContinueWith(t => Console.WriteLine(t.Result), TaskContinuationOptions.ExecuteSynchronously);
                    s1.Send("Late");
                    await t1;
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
                    StreamSink<string> s1 = new StreamSink<string>();
                    Task<string> t1 = s1.ListenOnce();

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
                    StreamSink<string> sa = new StreamSink<string>();
                    StreamSink<string> sb = new StreamSink<string>();
                    Task<string> ta = sa.ListenOnce();
                    Task<string> tb = sb.ListenOnce();
                    Func<Task<string>> t = async () => await ta + " " + await tb;
                    sa.Send("Hello");
                    Func<Task> task = () => t().ContinueWith(t1 => Console.WriteLine(t1.Result), TaskContinuationOptions.ExecuteSynchronously);
                    sb.Send("World");
                    await task();
                }

                Console.WriteLine();

                Console.WriteLine("*** Simultaneous case");
                {
                    StreamSink<string> sa = new StreamSink<string>();
                    StreamSink<string> sb = new StreamSink<string>();
                    Task<string> ta = sa.ListenOnce();
                    Task<string> tb = sb.ListenOnce();
                    Func<Task<string>> t = async () => await ta + " " + await tb;
                    Func<Task> task = () => t().ContinueWith(t1 => Console.WriteLine(t1.Result), TaskContinuationOptions.ExecuteSynchronously);
                    Transaction.RunVoid(() =>
                    {
                        sa.Send("Hello");
                        sb.Send("World");
                    });
                    await task();
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
                    StreamSink<string> sa = new StreamSink<string>();
                    StreamSink<string> sb = new StreamSink<string>();
                    Task<string> ta = sa.ListenOnce();
                    Task<string> tb = sb.ListenOnce();

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
                    StreamSink<string> sa = new StreamSink<string>();
                    StreamSink<string> sb = new StreamSink<string>();
                    Task<string> ta = sa.ListenOnce();
                    Task<string> tb = sb.ListenOnce();

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