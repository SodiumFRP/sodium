using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

using NUnit.Framework;

using Sodium;

using Strilanc.Value;

namespace Tests.sodium
{
  [TestFixture]
  public class EventTester
  {
    [TearDown]
    public void TearDown()
    {
      GC.Collect();
      Thread.Sleep(100);
    }

    [Test]
    public void TestSendEvent()
    {
      var e = new EventSink<int>();
      var @out = new List<int>();
      Listener l = e.Listen(x => { @out.Add(x); });
      e.Send(5);
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 5 }, @out);
      e.Send(6);
      CollectionAssert.AreEqual(new[] { 5 }, @out);
    }

    [Test]
    public void TestMap()
    {
      var e = new EventSink<int>();
      Event<String> m = e.Map(x => x.ToString());
      var @out = new List<string>();
      Listener l = m.Listen(x => { @out.Add(x); });
      e.Send(5);
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { "5" }, @out);
    }

    [Test]
    public void TestMergeNonSimultaneous()
    {
      var e1 = new EventSink<int>();
      var e2 = new EventSink<int>();
      var @out = new List<int>();
      Listener l = Event<int>.Merge(e1, e2).Listen(x => { @out.Add(x); });
      e1.Send(7);
      e2.Send(9);
      e1.Send(8);
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 7, 9, 8 }, @out);
    }

    [Test]
    public void TestMergeSimultaneous()
    {
      var e = new EventSink<int>();
      var @out = new List<int>();
      Listener l = Event<int>.Merge(e, e).Listen(x => { @out.Add(x); });
      e.Send(7);
      e.Send(9);
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 7, 7, 9, 9 }, @out);
    }

    [Test]
    public void TestMergeLeftBias()
    {
      var e1 = new EventSink<string>();
      var e2 = new EventSink<string>();
      var @out = new List<string>();
      Listener l = Event<string>.Merge(e1, e2).Listen(x => { @out.Add(x); });
      Transaction.RunVoid(() =>
      {
        e1.Send("left1a");
        e1.Send("left1b");
        e2.Send("right1a");
        e2.Send("right1b");
      });
      Transaction.RunVoid(() =>
      {
        e2.Send("right2a");
        e2.Send("right2b");
        e1.Send("left2a");
        e1.Send("left2b");
      });
      l.Unlisten();
      CollectionAssert.AreEqual(new[]{
              "left1a", "left1b",
              "right1a", "right1b",
              "left2a", "left2b",
              "right2a", "right2b"
          }, @out);
    }

    [Test]
    public void TestCoalesce()
    {
      var e1 = new EventSink<int>();
      var e2 = new EventSink<int>();
      var @out = new List<int>();
      Listener l =
           Event<int>.Merge<int>(e1, Event<int>.Merge<int>(e1.Map<int>(x => x * 100), e2))
          .Coalesce((a, b) => a + b)
          .Listen(x => { @out.Add(x); });
      e1.Send(2);
      e1.Send(8);
      e2.Send(40);
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 202, 808, 40 }, @out.ToArray());
    }

    [Test]
    public void TestFilter()
    {
      var e = new EventSink<char>();
      var @out = new List<char>();
      Listener l = e.Filter(c => Char.IsUpper((char)c)).Listen(c => { @out.Add(c); });
      e.Send('H');
      e.Send('o');
      e.Send('I');
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 'H', 'I' }, @out);
    }

    [Test]
    public void TestFilterNotNull()
    {
      var e = new EventSink<string>();
      var @out = new List<string>();
      Listener l = e.FilterNotNull().Listen(s => { @out.Add(s); });
      e.Send("tomato");
      e.Send(null);
      e.Send("peach");
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { "tomato", "peach" }, @out);
    }

    [Test]
    public void testFilterOptional()
    {
      EventSink<Optional<String>> e = new EventSink<Optional<String>>();
      List<String> @out = new List<string>();
      Listener l = Event<Optional<String>>.FilterOptional(e).Listen(s => { @out.Add(s); });
      e.Send(new Optional<string> ("tomato"));
      e.Send(new Optional<string>.Empty());
      e.Send(new Optional<string>("peach"));
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { "tomato", "peach" }, @out);
    }

    [Test]
    public void TestLoopEvent()
    {
      var ea = new EventSink<int>();
      Event<int> ec = Transaction.Run(() =>
      {
        var eb = new EventLoop<int>();
        Event<int> ec_ = Event<int>.MergeWith<int>((x, y) => x + y, ea.Map<int>(x => x % 10), eb);
        Event<int> ebOut = ea.Map<int>(x => x / 10).Filter(x => x != 0);
        eb.Loop(ebOut);
        return ec_;
      });
      var @out = new List<int>();
      Listener l = ec.Listen(x => { @out.Add(x); });
      ea.Send(2);
      ea.Send(52);
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 2, 7 }, @out.ToArray());
    }

    [Test]
    public void TestGate()
    {
      var ec = new EventSink<char?>();
      var epred = new BehaviorSink<Boolean>(true);
      var @out = new List<char?>();
      Listener l = ec.Gate(epred).Listen(x => { @out.Add(x); });
      ec.Send('H');
      epred.Send(false);
      ec.Send('O');
      epred.Send(true);
      ec.Send('I');
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 'H', 'I' }, @out);
    }

    [Test]
    public void TestCollect()
    {
      var ea = new EventSink<int>();
      var @out = new List<int>();
      Event<int> sum = ea.Collect(
        (int)100,
        //(a,s) => new Tuple2(a+s, a+s)
        (a, s) => new Tuple<int, int>(a + s, a + s));
      Listener l = sum.Listen(x => { @out.Add(x); });
      ea.Send(5);
      ea.Send(7);
      ea.Send(1);
      ea.Send(2);
      ea.Send(3);
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 105, 112, 113, 115, 118 }, @out);
    }

    [Test]
    public void TestAccum()
    {
      var ea = new EventSink<int>();
      var @out = new List<int>();
      Behavior<int> sum = ea.Accum((int)100, (a, s) => a + s);
      Listener l = sum.Updates().Listen(x => { @out.Add(x); });
      ea.Send(5);
      ea.Send(7);
      ea.Send(1);
      ea.Send(2);
      ea.Send(3);
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 105, 112, 113, 115, 118 }, @out);
    }

    [Test]
    public void TestOnce()
    {
      var e = new EventSink<char>();
      var @out = new List<char>();
      Listener l = e.Once().Listen(x => { @out.Add(x); });
      e.Send('A');
      e.Send('B');
      e.Send('C');
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 'A' }, @out);
    }

    [Test]
    public void TestDelay()
    {
      var e = new EventSink<char>();
      Behavior<char> b = e.Hold(' ');
      var @out = new List<char>();
      Listener l = e.Delay().Snapshot(b).Listen(x => { @out.Add(x); });
      e.Send('C');
      e.Send('B');
      e.Send('A');
      l.Unlisten();
      CollectionAssert.AreEqual(new[] { 'C', 'B', 'A' }, @out);
    }
  }

}