using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace Sodium.Tests
{
    [TestFixture]
    public class DiscreteCellTests
    {
        [Test]
        public void TestLoop()
        {
            Tuple<DiscreteCell<int>, DiscreteCellStreamSink<int>> result = Transaction.Run(() =>
             {
                 DiscreteCellLoop<int> loop = DiscreteCell.CreateLoop<int>();
                 DiscreteCell<int> cLocal = loop.Map(v => v * 5);
                 DiscreteCellStreamSink<int> sLocal = new DiscreteCellStreamSink<int>();
                 loop.Loop(sLocal.Hold(3));
                 return Tuple.Create(cLocal, sLocal);
             });

            DiscreteCell<int> c = result.Item1;
            DiscreteCellStreamSink<int> s = result.Item2;

            List<int> output1 = new List<int>();
            List<int> output2 = new List<int>();
            IListener l = c.Listen(output1.Add);
            IListener l2 = c.Updates.Listen(output2.Add);

            s.Send(5);
            s.Send(7);

            l2.Unlisten();
            l.Unlisten();

            CollectionAssert.AreEqual(new[] { 15, 25, 35 }, output1);
            CollectionAssert.AreEqual(new[] { 25, 35 }, output2);
        }
    }
}