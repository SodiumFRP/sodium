using System;
using System.Collections.Generic;
using JetBrains.dotMemoryUnit;
using NUnit.Framework;

namespace Sodium.Tests.Memory
{
    [TestFixture]
    public class StreamTests
    {
        [Test]
        [Ignore("Requires dotMemory.")]
        public void TestStreamGarbageCollection()
        {
            int? beforeListenerCount = null;
            int? duringListenerCount = 3;
            int? duringListenerCount2 = 3;
            int? afterListenerCount = null;

            ((Action)(() =>
            {
                IDisposable listener = null;

                StreamSink<int> s = new StreamSink<int>();

                dotMemory.Check(memory => beforeListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);

                ((Action)(() =>
                {
                    Stream<string> m = s.Map(x => (x + 2).ToString());
                    List<string> @out = new List<string>();

                    listener = m.Listen(@out.Add);

                    dotMemory.Check(memory => duringListenerCount = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
                }))();

                dotMemory.Check(memory => duringListenerCount2 = memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);

                using (listener)
                {
                }
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

            StreamSink<int> s = new StreamSink<int>();
            Stream<string> m = s.Map(x => (x + 2).ToString());
            List<string> @out = new List<string>();

            dotMemory.Check(memory => beforeListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);

            ((Action)(() =>
            {
                using (m.Listen(@out.Add))
                {
                    dotMemory.Check(memory => duringListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
                    s.Send(5);
                    s.Send(3);
                }
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

            StreamSink<int> s = new StreamSink<int>();
            List<string> @out = new List<string>();

            dotMemory.Check(memory => beforeStreamCount = memory.GetObjects(where => where.Type.Is<Stream<int>>()).ObjectsCount + memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
            dotMemory.Check(memory => beforeListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);

            ((Action)(() =>
            {
                Stream<string> m = s.Map(x => x + 2).Map(x => 2 * x).Map(x => x + 1).Map(x => x.ToString());
                using (m.Listen(@out.Add))
                {
                    dotMemory.Check(memory => duringStreamCount = memory.GetObjects(where => where.Type.Is<Stream<int>>()).ObjectsCount + memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
                    dotMemory.Check(memory => duringListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);
                    s.Send(5);
                    s.Send(3);
                }
                CollectionAssert.AreEqual(new[] { "15", "11" }, @out);
            }))();

            dotMemory.Check(memory => afterStreamCount = memory.GetObjects(where => where.Type.Is<Stream<int>>()).ObjectsCount + memory.GetObjects(where => where.Type.Is<Stream<string>>()).ObjectsCount);
            dotMemory.Check(memory => afterListenerCount = memory.GetObjects(where => where.Interface.Is<IListener>()).ObjectsCount);

            // although all listeners and streams have been cleand up, the nodes will not be disconnected until the stream fires next
            Assert.AreEqual(1, s.Node.GetListeners().Count);
            s.Send(1);
            Assert.AreEqual(0, s.Node.GetListeners().Count);

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