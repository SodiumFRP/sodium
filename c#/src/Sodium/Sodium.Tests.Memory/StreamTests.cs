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
    }
}