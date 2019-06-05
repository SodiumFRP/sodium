using System;
using System.Collections.Generic;
using JetBrains.dotMemoryUnit;
using NUnit.Framework;

namespace Sodium.Frp.Tests.Memory
{
    [TestFixture]
    public class StreamTests
    {
        [Test]
        [Ignore("Requires dotMemory.")]
        public void TestListen()
        {
            int? listenerCount = null;
            int? listenerCount2 = null;
            int? listenerCount3 = null;
            int? listenerCount4 = null;
            int? beforeListenerCount = null;
            int? duringListenerCount = null;
            int? duringStreamCount = null;
            int? afterListenerCount = null;

            ((Action)(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();

                dotMemory.Check(memory => listenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
                dotMemory.Check(memory => beforeListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);

                Stream<string> m = s.Map(x => (x + 2).ToString());
                List<string> @out = new List<string>();

                ((Action)(() =>
                {
                    // ReSharper disable once UnusedVariable
                    IListener listener = m.Listen(@out.Add);

                    dotMemory.Check(memory => listenerCount2 = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
                    dotMemory.Check(memory => duringListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
                }))();

                dotMemory.Check(memory => listenerCount3 = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
                dotMemory.Check(memory => duringStreamCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
            }))();

            dotMemory.Check(memory => listenerCount4 = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
            dotMemory.Check(memory => afterListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);

            Assert.IsNotNull(beforeListenerCount);
            Assert.IsNotNull(listenerCount);
            Assert.IsNotNull(listenerCount2);
            Assert.IsNotNull(duringListenerCount);
            Assert.IsNotNull(duringStreamCount);
            Assert.IsNotNull(afterListenerCount);

            Assert.AreEqual(listenerCount, listenerCount4, "BeforeL == AfterL");
            Assert.IsTrue(listenerCount2 > listenerCount3, "DuringL > AfterL");
            Assert.IsTrue(listenerCount2 > listenerCount, "DuringL > BeforeL");

            Assert.AreEqual(beforeListenerCount, afterListenerCount, "Before == After");
            Assert.AreEqual(duringListenerCount, duringStreamCount, "During == During2");
            Assert.IsTrue(duringListenerCount > beforeListenerCount, "During > Before");
        }

        [Test]
        [Ignore("Requires dotMemory.")]
        public void TestUnlisten()
        {
            int? listenerCount = null;
            int? listenerCount2 = null;
            int? listenerCount3 = null;
            int? listenerCount4 = null;
            int? beforeListenerCount = null;
            int? duringListenerCount = null;
            int? duringStreamCount = null;
            int? afterListenerCount = null;

            ((Action)(() =>
            {
                StreamSink<int> s = Stream.CreateSink<int>();

                dotMemory.Check(memory => listenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
                dotMemory.Check(memory => beforeListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);

                Stream<string> m = s.Map(x => (x + 2).ToString());
                List<string> @out = new List<string>();

                ((Action)(() =>
                {
                    IListener listener = m.Listen(@out.Add);

                    listener.Unlisten();

                    dotMemory.Check(memory => listenerCount2 = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
                    dotMemory.Check(memory => duringListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
                }))();

                dotMemory.Check(memory => listenerCount3 = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
                dotMemory.Check(memory => duringStreamCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
            }))();

            dotMemory.Check(memory => listenerCount4 = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
            dotMemory.Check(memory => afterListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);

            Assert.IsNotNull(beforeListenerCount);
            Assert.IsNotNull(listenerCount);
            Assert.IsNotNull(listenerCount2);
            Assert.IsNotNull(duringListenerCount);
            Assert.IsNotNull(duringStreamCount);
            Assert.IsNotNull(afterListenerCount);

            Assert.AreEqual(listenerCount, listenerCount4, "BeforeL == After2L");
            Assert.IsTrue(listenerCount2 > listenerCount3, "DuringL > AfterL");
            Assert.IsTrue(listenerCount2 > listenerCount, "DuringL > BeforeL");

            Assert.AreEqual(beforeListenerCount, afterListenerCount, "Before == After");
            Assert.AreEqual(duringListenerCount, duringStreamCount, "During == During2");
            Assert.IsTrue(duringListenerCount > beforeListenerCount, "During > Before");
        }

        [Test]
        [Ignore("Requires dotMemory.")]
        public void TestStreamGarbageCollection()
        {
            int? beforeListenerCount = null;
            int? duringListenerCount = null;
            int? duringListenerCount2 = null;
            int? afterListenerCount = null;

            ((Action)(() =>
            {
                // ReSharper disable once NotAccessedVariable
                IListener listener = null;

                StreamSink<int> s = Stream.CreateSink<int>();

                dotMemory.Check(memory => beforeListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);

                ((Action)(() =>
                {
                    Stream<string> m = s.Map(x => (x + 2).ToString());
                    List<string> @out = new List<string>();

                    listener = m.Listen(@out.Add);

                    dotMemory.Check(memory => duringListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
                }))();

                dotMemory.Check(memory => duringListenerCount2 = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
            }))();

            dotMemory.Check(memory => afterListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);

            Assert.IsNotNull(beforeListenerCount);
            Assert.IsNotNull(duringListenerCount);
            Assert.IsNotNull(duringListenerCount2);
            Assert.IsNotNull(afterListenerCount);

            Assert.AreEqual(beforeListenerCount, afterListenerCount, "Before == After");
            Assert.AreEqual(duringListenerCount, duringListenerCount2, "During == During2");
            Assert.IsTrue(duringListenerCount > beforeListenerCount, "During > Before");
        }

        [Test]
        [Ignore("Requires dotMemory.")]
        public void TestMapMemory()
        {
            int? beforeListenerCount = null;
            int? duringListenerCount = null;
            int? afterListenerCount = null;

            StreamSink<int> s = Stream.CreateSink<int>();
            Stream<string> m = s.Map(x => (x + 2).ToString());
            List<string> @out = new List<string>();

            dotMemory.Check(memory => beforeListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);

            ((Action)(() =>
            {
                IListener l = m.Listen(@out.Add);
                dotMemory.Check(memory => duringListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
                s.Send(5);
                s.Send(3);
                l.Unlisten();
                CollectionAssert.AreEqual(new[] { "7", "5" }, @out);
            }))();

            dotMemory.Check(memory => afterListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);

            Assert.IsNotNull(beforeListenerCount);
            Assert.IsNotNull(duringListenerCount);
            Assert.IsNotNull(afterListenerCount);

            Assert.AreEqual(beforeListenerCount, afterListenerCount, "Before == After");
            Assert.IsTrue(duringListenerCount > beforeListenerCount, "During > Before");
        }

        [Test]
        [Ignore("Requires dotMemory.")]
        public void TestNestedMapGarbageCollection()
        {
            int? beforeStreamCount = null;
            int? beforeListenerCount = null;
            int? duringStreamCount = null;
            int? duringListenerCount = null;
            int? afterStreamCount = null;
            int? afterListenerCount = null;

            StreamSink<int> s = Stream.CreateSink<int>();
            List<string> @out = new List<string>();

            dotMemory.Check(memory => beforeStreamCount = memory.GetObjects(where => where.Type.Is<Stream<int>>()).ObjectsCount + memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
            dotMemory.Check(memory => beforeListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);

            ((Action)(() =>
            {
                Stream<string> m = s.Map(x => x + 2).Map(x => 2 * x).Map(x => x + 1).Map(x => x.ToString());
                IListener l = m.Listen(@out.Add);
                dotMemory.Check(memory => duringStreamCount = memory.GetObjects(where => where.Type.Is<Stream<int>>()).ObjectsCount + memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
                dotMemory.Check(memory => duringListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
                s.Send(5);
                s.Send(3);
                l.Unlisten();
                CollectionAssert.AreEqual(new[] { "15", "11" }, @out);
            }))();

            dotMemory.Check(memory => afterStreamCount = memory.GetObjects(where => where.Type.Is<Stream<int>>()).ObjectsCount + memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
            dotMemory.Check(memory => afterListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);

            // although all listeners and streams have been cleand up, the nodes will not be disconnected until the stream fires next
            Assert.AreEqual(1, s.Node.GetListenersCopy().Count);
            s.Send(1);
            Assert.AreEqual(0, s.Node.GetListenersCopy().Count);

            Assert.IsNotNull(beforeStreamCount);
            Assert.IsNotNull(beforeListenerCount);
            Assert.IsNotNull(duringStreamCount);
            Assert.IsNotNull(duringListenerCount);
            Assert.IsNotNull(afterStreamCount);
            Assert.IsNotNull(afterListenerCount);

            Assert.AreEqual(beforeStreamCount, afterStreamCount, "Before Streams == After Streams");
            Assert.AreEqual(beforeListenerCount, afterListenerCount, "Before Listeners == After Listeners");
            Assert.IsTrue(duringStreamCount > beforeStreamCount, "During Streams > Before Streams");
            Assert.IsTrue(duringListenerCount > beforeListenerCount, "During Listeners > Before Listeners");
        }
    }
}