using NUnit.Framework;
using Sodium.Functional;

namespace Sodium.Frp.Tests.Internal
{
    [TestFixture]
    public class NodeTests
    {
        [Test]
        public void TestNode()
        {
            Node<int> a = new Node<int>();
            Node<int> b = new Node<int>();
            TransactionInternal.Apply(
                (trans, _) =>
                {
                    a.Link(trans, (t, v) => { }, b);
                    trans.Prioritized(a, t => { });
                    return UnitInternal.Value;
                },
                false);
            Assert.That(a.Rank, Is.LessThan(b.Rank));
        }

        [Test]
        public void TestDependency()
        {
            StreamSink<int> streamSink = Stream.CreateSink<int>();
            Stream<int> stream = streamSink.Map(v => v * 2);

            Assert.That(streamSink.Node.Rank, Is.LessThan(stream.Node.Rank));
        }

        [Test]
        public void TestSnapshot()
        {
            CellSink<int> cellSink = Cell.CreateSink(0);
            StreamSink<Unit> streamSink = Stream.CreateSink<Unit>();

            Cell<int> cell =
                cellSink.Map(
                        n =>
                        {
                            Cell<int> c = Cell.Constant(0);

                            if (n > 0)
                            {
                                for (int i = 0; i < 50; i++)
                                {
                                    c = c.Map(v => v);
                                }
                            }

                            return n > 1 ? c : streamSink.Snapshot(c).Hold(0);
                        })
                    .SwitchC();

            long rank1 = cell.UpdatesImpl.Node.Rank;

            cellSink.Send(1);
            
            long rank2 = cell.UpdatesImpl.Node.Rank;
            
            cellSink.Send(2);
            
            long rank3 = cell.UpdatesImpl.Node.Rank;

            Assert.That(rank1, Is.EqualTo(rank2));
            Assert.That(rank2, Is.LessThan(rank3));
        }
    }
}