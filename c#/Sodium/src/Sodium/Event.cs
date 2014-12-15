
using System;
using System.Collections.Generic;


namespace Sodium
{
  public class Event<TA>
  {
    private class ListenerImplementation<TA> : Listener, IDisposable
    {
      private readonly Event<TA> _event;
      private readonly TransactionHandler<TA> action;
      private readonly Node target;

      public ListenerImplementation(Event<TA> @event, TransactionHandler<TA> action, Node target)
      {
        _event = @event;
        this.action = action;
        this.target = target;

        Unlisten = UnlistenImpl;
      }

      private void UnlistenImpl()
      {
        lock (Transaction.ListenersLock)
        {
          _event.Listeners.Remove(action);
          _event.Node.UnlinkTo(target);
        }
      }

      public void Dispose()
      {
        Unlisten();
      }
    }

    protected readonly List<TransactionHandler<TA>> Listeners = new List<TransactionHandler<TA>>();
    readonly List<Listener> finalizers = new List<Listener>();
    public readonly Node Node = new Node(0L);
    protected readonly List<TA> Firings = new List<TA>();

    /// An event that never fires.
    public Event()
    {
    }

    public Func<object[]> SampleNow = () => null;

    ///  Listen for firings of this event. The returned Listener has an unlisten()
    ///  method to cause the listener to be removed. This is the observer pattern.

    public Listener Listen(Action<TA> action)
    {
      return Listen(new Handler<TA>() { Run = action });
    }

    public Listener Listen(Handler<TA> action)
    {
      return Listen_(Node.Null, new TransactionHandler<TA> { Run = (trans2, a) => action.Run(a) });
    }

    public Listener Listen_(Node target, TransactionHandler<TA> action)
    {
      return Transaction.Apply(trans1 => Listen(target, trans1, action, false));
    }

    public Listener Listen(Node target, Transaction trans, TransactionHandler<TA> action, bool suppressEarlierFirings)
    {
      lock (Transaction.ListenersLock)
      {
        if (Node.LinkTo(target))
          trans.ToRegen = true;
        Listeners.Add(action);
      }
      trans.Prioritized(target, new Handler<Transaction>
      {
        Run = trans2 =>
        {
          object[] aNow = SampleNow();
          if (aNow != null)
          {
            // In cases like value(), we start with an initial value.
            foreach (object t in aNow)
              action.Run(trans, (TA)t); // <-- unchecked warning is here
          }
          if (!suppressEarlierFirings)
          {
            // Anything sent already in this transaction must be sent now so that
            // there's no order dependency between send and listen.
            foreach (TA a in Firings)
              action.Run(trans, a);
          }
        }
      });
      return new ListenerImplementation<TA>(this, action, target);
    }

    ///
    ///Transform the event's value according to the supplied function.
    ///
    public Event<TB> Map<TB>(Func<TA, TB> f)
    {
      Event<TA> ev = this;
      var @out = new EventSink<TB>
      {

        SampleNow = () =>
              {
                object[] oi = ev.SampleNow();
                if (oi != null)
                {
                  var oo = new Object[oi.Length];
                  for (int i = 0; i < oo.Length; i++)
                    oo[i] = f((TA)oi[i]);
                  return oo;
                }
                return null;
              }
      };
      Listener l = Listen_(@out.Node, new TransactionHandler<TA>
      {
        Run = (trans2, a) => @out.Send(trans2, f(a))
      });
      return @out.AddCleanup(l);
    }

    ///
    ///Create a behavior with the specified initial value, that gets updated
    ///by the values coming through the event. The 'current value' of the behavior
    ///is notionally the value as it was 'at the start of the transaction'.
    ///That is, state updates caused by event firings get processed at the end of
    ///the transaction.
    ///
    public Behavior<TA> Hold(TA initValue)
    {
      return Transaction.Apply(trans => new Behavior<TA>(LastFiringOnly(trans), initValue));
    }

    public Behavior<TA> HoldLazy(Func<TA> initValue)
    {
      return Transaction.Apply(trans => new LazyBehavior<TA>(LastFiringOnly(trans), initValue));
    }

    ///
    ///Variant of snapshot that throws away the event's value and captures the behavior's.
    ///
    public Event<TB> Snapshot<TB>(Behavior<TB> beh) 
    {
      return Snapshot(beh, (a, b) => b);
    }

    ///
    ///Sample the behavior at the time of the event firing. Note that the 'current value'
    /// of the behavior that's sampled is the value as at the start of the transaction
    /// before any state changes of the current transaction are applied through 'hold's.
    ///
    public Event<TC> Snapshot<TB, TC>(Behavior<TB> b, Func<TA, TB, TC> f)
    {
      Event<TA> ev = this;
      var @out = new EventSink<TC>
      {
        SampleNow = () =>
        {
          Object[] oi = ev.SampleNow();
          if (oi != null)
          {
            var oo = new Object[oi.Length];
            for (int i = 0; i < oo.Length; i++)
              oo[i] = f((TA)oi[i], b.SampleNoTrans());
            return oo;
          }
          return null;
        }
      };
      Listener l = Listen_(
        @out.Node,
        new TransactionHandler<TA>
        {
          Run = (trans2, a) => @out.Send(trans2, f(a, b.SampleNoTrans()))
        });
      return @out.AddCleanup(l);
    }

    /**
     * Merge two streams of events of the same type.
     *
     * In the case where two event occurrences are simultaneous (i.e. both
     * within the same transaction), both will be delivered in the same
     * transaction. If the event firings are ordered for some reason, then
     * their ordering is retained. In many common cases the ordering will
     * be undefined.
     */
	public Event<TA> Merge<TA>(Event<TA> eb) 
	{
	    return Event<TA>.Merge<TA>(this as Event<TA>, eb);
	}

    ///
    ///Merge two streams of events of the same type.
    ///
    ///In the case where two event occurrences are simultaneous (i.e. both
    ///within the same transaction), both will be delivered in the same
    ///transaction. If the event firings are ordered for some reason, then
    ///their ordering is retained. In many common cases the ordering will
    ///be undefined.
    ///
    public static Event<TA> Merge<TA>(Event<TA> ea, Event<TA> eb) 
    {
      var @out = new EventSink<TA>
      {
        SampleNow = () =>
            {
              Object[] oa = ea.SampleNow();
              Object[] ob = eb.SampleNow();
              if (oa != null && ob != null)
              {
                var oo = new Object[oa.Length + ob.Length];
                int j = 0;
                for (int i = 0; i < oa.Length; i++) oo[j++] = oa[i];
                for (int i = 0; i < ob.Length; i++) oo[j++] = ob[i];
                return oo;
              }
              if (oa != null)
                return oa;
              return ob;
            }
      };
      var h = new TransactionHandler<TA>
      {
        Run = (trans, a) => @out.Send(trans, a)
      };
      Listener l1 = ea.Listen_(@out.Node, h);
      Listener l2 = eb.Listen_(@out.Node, new TransactionHandler<TA>
      {
        Run = (trans1, a) => trans1.Prioritized(@out.Node, new Handler<Transaction>
        {
          Run = trans2 =>
          {
            @out.Send(trans2, a);
          }
        })
      });
      return @out.AddCleanup(l1).AddCleanup(l2);
    }

    ///
    ///Push each event occurrence onto a new transaction.
    ///
    public Event<TA> Delay()
    {
      var @out = new EventSink<TA>();
      Listener l1 = Listen_(@out.Node, new TransactionHandler<TA>
      {
        Run = (trans, a) => trans.Post(new Runnable
        {
          Run = () =>
          {
            var trans1 = new Transaction();
            try
            {
              @out.Send(trans1, a);
            }
            finally
            {
              trans1.Close();
            }
          }
        })
      });
      return @out.AddCleanup(l1);
    }

    ///
    ///If there's more than one firing in a single transaction, combine them into
    ///one using the specified combining function.
    ///
    ///If the event firings are ordered, then the first will appear at the left
    ///input of the combining function. In most common cases it's best not to
    ///make any assumptions about the ordering, and the combining function would
    ///ideally be commutative.
    ///
    public Event<TA> Coalesce(Func<TA, TA, TA> f)
    {
      return Transaction.Apply(trans => Coalesce(trans, f));
    }

    Event<TA> Coalesce(Transaction trans1, Func<TA, TA, TA> f)
    {
      Event<TA> ev = this;
      var @out = new EventSink<TA>
      {
        SampleNow = () =>
        {
          object[] oi = ev.SampleNow();
          if (oi != null)
          {
            var o = (TA)oi[0];
            for (int i = 1; i < oi.Length; i++)
              o = f(o, (TA)oi[i]);
            return new object[] { o };
          }
          return null;
        }
      };
      TransactionHandler<TA> h = new CoalesceHandler<TA>(f, @out);
      Listener l = Listen(@out.Node, trans1, h, false);
      return @out.AddCleanup(l);
    }

    ///
    ///Clean up the output by discarding any firing other than the last one. 
    ///
    public Event<TA> LastFiringOnly(Transaction trans)
    {
      return Coalesce(trans, (first, second) => second);
    }

    ///
    ///Merge two streams of events of the same type, combining simultaneous
    ///event occurrences.
    ///
    ///In the case where multiple event occurrences are simultaneous (i.e. all
    ///within the same transaction), they are combined using the same logic as
    ///'coalesce'.
    ///
    public static Event<TA> MergeWith<TA>(Func<TA, TA, TA> f, Event<TA> ea, Event<TA> eb)
    {
      return Merge(ea, eb).Coalesce(f);
    }

    ///
    ///Only keep event occurrences for which the predicate returns true.
    ///
    public Event<TA> Filter(Func<TA, bool> f)
    {
      Event<TA> ev = this;
      var @out = new EventSink<TA>
      {
        SampleNow = () =>
            {
              object[] oi = ev.SampleNow();
              if (oi != null)
              {
                var oo = new object[oi.Length];
                int j = 0;
                for (int i = 0; i < oi.Length; i++)
                  if (f((TA)oi[i]))
                    oo[j++] = oi[i];
                if (j == 0)
                  oo = null;
                else
                  if (j < oo.Length)
                  {
                    var oo2 = new object[j];
                    for (int i = 0; i < j; i++)
                      oo2[i] = oo[i];
                    oo = oo2;
                  }
                return oo;
              }
              return null;
            }
      };
      Listener l = Listen_(@out.Node, new TransactionHandler<TA>
      {
        Run = (trans2, a) =>
        {
          if (f(a)) @out.Send(trans2, a);
        }
      });
      return @out.AddCleanup(l);
    }

    ///
    ///Filter out any event occurrences whose value is a Java null pointer.
    ///
    public Event<TA> FilterNotNull()
    {
      return Filter(a => a != null);
    }

    ///
    ///Filter the empty values out, and strip the Optional wrapper from the present ones.
    ///
    public static Event<TA> FilterOptional<TA>(Event<Optional<TA>> ev)
    {
      var @out = new EventSink<TA>
      {
        SampleNow = () =>
          {
            object[] oi = ev.SampleNow();
            if (oi != null)
            {
              var oo = new object[oi.Length];
              int j = 0;
              for (int i = 0; i < oi.Length; i++)
              {
                var oa = (Optional<TA>)oi[i];
                if (oa.IsPresent)
                  oo[j++] = oa.Get();
              }
              if (j == 0)
                oo = null;
              else
                if (j < oo.Length)
                {
                  var oo2 = new object[j];
                  for (int i = 0; i < j; i++)
                    oo2[i] = oo[i];
                  oo = oo2;
                }
              return oo;
            }
            return null;
          }
      };
      Listener l = ev.Listen_(@out.Node, new TransactionHandler<Optional<TA>>
      {
        Run = (trans2, oa) =>
        {
          if (oa.IsPresent) @out.Send(trans2, oa.Get());
        }
      });
      return @out.AddCleanup(l);
    }

    ///
    ///Let event occurrences through only when the behavior's value is True.
    ///Note that the behavior's value is as it was at the start of the transaction,
    ///that is, no state changes from the current transaction are taken into account.
    ///
    public Event<TA> Gate(Behavior<Boolean> bPred)
    {
      return Snapshot(bPred, (a, pred) => pred ? a : default(TA)).FilterNotNull();
    }

    ///
    ///Transform an event with a generalized state loop (a mealy machine). The function
    ///is passed the input and the old state and returns the new state and output value.
    ///
    public Event<TB> Collect<TB, TS>(TS initState, Func<TA, TS, Tuple<TB, TS>> f)
    {
      return Transaction.Run(() =>
      {
        Event<TA> ea = this;
        var es = new EventLoop<TS>();
        Behavior<TS> s = es.Hold(initState);
        Event<Tuple<TB, TS>> ebs = ea.Snapshot(s, f);
        Event<TB> eb = ebs.Map(bs => bs.Item1);
        Event<TS> esOut = ebs.Map(bs => bs.Item2);
        es.Loop(esOut);
        return eb;
      });
    }

    ///
    ///Accumulate on input event, outputting the new state each time.
    ///
    public Behavior<TS> Accum<TS>(TS initState, Func<TA, TS, TS> f) 
    {
      return Transaction.Run(() =>
      {
        Event<TA> ea = this;
        var es = new EventLoop<TS>();
        Behavior<TS> s = es.Hold(initState);
        Event<TS> esOut = ea.Snapshot(s, f);
        es.Loop(esOut);
        return esOut.Hold(initState);
      });
    }

    ///
    ///Throw away all event occurrences except for the first one.
    ///
    public Event<TA> Once()
    {
      // This is a bit long-winded but it's efficient because it deregisters
      // the listener.
      Event<TA> ev = this;
      var la = new Listener[1];
      var @out = new EventSink<TA>
      {
        SampleNow = () =>
        {
          object[] oi = ev.SampleNow();
          object[] oo = oi;
          if (oo != null)
          {
            if (oo.Length > 1)
              oo = new[] { oi[0] };
            if (la[0] != null)
            {
              la[0].Unlisten();
              la[0] = null;
            }
          }
          return oo;
        }
      };
      la[0] = ev.Listen_(@out.Node, new TransactionHandler<TA>
      {
        Run = (trans, a) =>
        {
          @out.Send(trans, a);
          if (la[0] != null)
          {
            la[0].Unlisten();
            la[0] = null;
          }
        }
      });
      return @out.AddCleanup(la[0]);
    }

    public Event<TA> AddCleanup(Listener cleanup)
    {
      finalizers.Add(cleanup);
      return this;
    }

    protected void Dispose()
    {
      foreach (Listener l in finalizers)
        l.Unlisten();
    }

  }

  public class CoalesceHandler<TA> : TransactionHandler<TA>
  {
    public CoalesceHandler(Func<TA, TA, TA> f, EventSink<TA> @out)
    {
      this.f = f;
      this.@out = @out;
      Run = (trans1, a) =>
      {
        if (accumValid)
          accum = f(accum, a);
        else
        {
          CoalesceHandler<TA> thiz = this;
          trans1.Prioritized(
            @out.Node,
            new Handler<Transaction>
            {
              Run = trans2 =>
              {
                @out.Send(trans2, thiz.accum);
                thiz.accumValid = false;
                thiz.accum = default(TA);
              }
            });
          accum = a;
          accumValid = true;
        }
      };
    }

    private Func<TA, TA, TA> f;
    private EventSink<TA> @out;
    private bool accumValid = false;
    private TA accum;
  }
}

