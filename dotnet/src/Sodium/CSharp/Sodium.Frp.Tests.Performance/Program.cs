using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Sodium.Functional;

namespace Sodium.Frp.Tests.Performance
{
    internal class Program
    {
        public static void Main3(string[] args)
        {
            CellSink<bool> c = Cell.CreateSink(false);

            Console.WriteLine("Press any key");
            Console.ReadKey();

            ((Action)(() =>
            {
                List<Cell<bool>> cc = new List<Cell<bool>>();
                for (int i = 0; i < 5000; i++)
                {
                    cc.Add(c.Map(v => !v));
                }

                Console.WriteLine("Press any key");
                Console.ReadKey();
            }))();

            Console.WriteLine("Press any key");
            Console.ReadKey();
        }

        public static void Main2(string[] args)
        {
            Console.WriteLine("Press any key");
            Console.ReadKey();

            //CellSink<IReadOnlyList<SmallTestObject>> s = ((Func<CellSink<IReadOnlyList<SmallTestObject>>>)(() =>
            //   new CellSink<IReadOnlyList<SmallTestObject>>(new SmallTestObject[0])))();
            CellSink<IReadOnlyList<SmallTestObject>> s = ((Func<CellSink<IReadOnlyList<SmallTestObject>>>)(() =>
                Cell.CreateSink<IReadOnlyList<SmallTestObject>>(Enumerable.Range(0, 500).Select(_ => new SmallTestObject()).ToArray())))();
            Cell<IReadOnlyList<bool>> s2 = s.Map(oo => oo.Select(o => o.S).Lift()).SwitchC();

            ((Action)(() =>
            {
                for (int i = 0; i < 5; i++)
                {
                    s.Send(Enumerable.Range(0, 500).Select(_ => new SmallTestObject()).ToArray());
                }
            }))();
            s.Send(new SmallTestObject[0]);

            Console.WriteLine("Press any key");
            Console.ReadKey();

            ((Action)(() =>
            {
                for (int i = 0; i < 5; i++)
                {
                    s.Send(Enumerable.Range(0, 500).Select(_ => new SmallTestObject()).ToArray());
                }
            }))();

            s.Send(new SmallTestObject[0]);

            Console.WriteLine("Press any key");
            Console.ReadKey();
        }

        private class SmallTestObject
        {
            public SmallTestObject()
            {
                this.S = Cell.CreateSink(false);
            }

            public CellSink<bool> S { get; }
        }

        public static void Main(string[] args)
        {
            Console.WriteLine("Press any key");
            Console.ReadKey();

            var (toggleAllSelectedStream, objectsAndIsSelected, selectAllStream, objects) = Transaction.Run(() =>
            {
                CellLoop<bool?> allSelectedCellLoop = Cell.CreateLoop<bool?>();
                StreamSink<Unit> toggleAllSelectedStreamLocal = Stream.CreateSink<Unit>();
                Stream<bool> selectAllStreamLocal = toggleAllSelectedStreamLocal.Snapshot(allSelectedCellLoop).Map(a => a != true);

                IReadOnlyList<TestObject> o2 = Enumerable.Range(0, 10000).Select(n => new TestObject(n, selectAllStreamLocal)).ToArray();
                CellSink<IReadOnlyList<TestObject>> objectsLocal =
                    Cell.CreateSink((IReadOnlyList<TestObject>)new TestObject[0]);

                var objectsAndIsSelectedLocal = objectsLocal.Map(oo => oo.Select(o => o.IsSelected.Map(s => new { Object = o, IsSelected = s })).Lift()).SwitchC();

                bool defaultValue = o2.Count < 1;
                Cell<bool?> allSelected =
                    objectsAndIsSelectedLocal.Map(
                        oo =>
                            !oo.Any()
                                ? defaultValue
                                : (oo.All(o => o.IsSelected)
                                    ? true
                                    : (oo.All(o => !o.IsSelected) ? (bool?)false : null)));
                allSelectedCellLoop.Loop(allSelected);

                return (toggleAllSelectedStreamLocal, objectsAndIsSelectedLocal, selectAllStreamLocal, objectsLocal);
            });

            // ReSharper disable once UnusedVariable
            IListener l = Transaction.Run(() => objectsAndIsSelected.Map(oo => oo.Count(o => o.IsSelected)).Updates().Listen(v => Console.WriteLine($"{v} selected")));

            Console.WriteLine("Press any key");
            Console.ReadKey();

            Stopwatch sw = new Stopwatch();
            sw.Start();

            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            Thread.Sleep(500);
            SendMore(objects, selectAllStream);
            objects.Sample()[2].IsSelectedStreamSink.Send(true);
            Transaction.RunVoid(() =>
            {
                objects.Sample()[3].IsSelectedStreamSink.Send(true);
                objects.Sample()[4].IsSelectedStreamSink.Send(true);
            });
            Transaction.RunVoid(() =>
            {
                objects.Send(Enumerable.Range(0, 2500).Select(n => new TestObject(n, selectAllStream)).ToArray());
                toggleAllSelectedStream.Send(Unit.Value);
            });
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            toggleAllSelectedStream.Send(Unit.Value);
            objects.Send(new TestObject[0]);

            sw.Stop();

            Console.WriteLine();
            Console.WriteLine();
            Console.WriteLine($"Elapsed: {sw.ElapsedMilliseconds}ms");

            Console.WriteLine();
            Console.WriteLine("Press any key");
            Console.ReadKey();
        }

        private static void SendMore(CellSink<IReadOnlyList<TestObject>> cellSink, Stream<bool> selectAllStream) =>
            Transaction.RunVoid(() => cellSink.Send(Enumerable.Range(0, 20000).Select(n => new TestObject(n, selectAllStream)).ToArray()));

        private class TestObject
        {
            public TestObject(int id, Stream<bool> selectAllStream)
            {
                this.Id = id;
                this.IsSelectedStreamSink = Stream.CreateSink<bool>();
                this.IsSelected = selectAllStream.OrElse(this.IsSelectedStreamSink).Hold(false);
            }

            public int Id { get; }
            public StreamSink<bool> IsSelectedStreamSink { get; }
            public Cell<bool> IsSelected { get; }
        }
    }
}