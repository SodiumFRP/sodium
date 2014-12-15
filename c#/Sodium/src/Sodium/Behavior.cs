using System;

namespace Sodium
{

  public class Behavior<TA> : IDisposable
  {
    protected readonly Event<TA> Event;
    TA _eventValue;
    protected bool _isEventValueSet;
    protected TA EventValue
    {
      get
      {
        if (!_isEventValueSet)
          throw new Exception("ValueUpdate is not set");
        return _eventValue;
      }
      set
      {
        _eventValue = value;
        _isEventValueSet = true;
      }
    }
    void ResetEventValue()
    {
      _isEventValueSet = false;
    }


    TA _valueUpdate;
    bool _isValueUpdateSet;

    TA ValueUpdate
    {
      get
      {
        if (!_isValueUpdateSet)
          throw new Exception("ValueUpdate is not set");
        return _valueUpdate;
      }
      set
      {
        _valueUpdate = value;
        _isValueUpdateSet = true;
      }
    }

    void ResetValueUpdate()
    {
      _isValueUpdateSet = false;
    }

    private Listener cleanup;
    protected Func<TA> LazyInitValue;  // Used by LazyBehavior

    ///
    ///A behavior with a constant value.
    ///
    public Behavior(TA value)
    {
      Event = new Event<TA>();
      EventValue = value;
    }

    public Behavior(Event<TA> @event, TA initValue, bool resetInitValue = false)
    {
      Event = @event;
      EventValue = initValue;
      if (resetInitValue) ResetEventValue();
      Transaction.Run(new Handler<Transaction>
      {
        Run = trans1 =>
        {
          cleanup = @event.Listen(Node.Null, trans1, new TransactionHandler<TA>
          {
            Run = (trans2, a) =>
            {
              if (!_isValueUpdateSet)
              {
                trans2.Last(new Runnable
                {
                  Run = () =>
                  {
                    EventValue = ValueUpdate;
                    LazyInitValue = null;
                    ResetValueUpdate();
                  }
                });
              }
              ValueUpdate = a;
            }
          }, false);
        }
      });
    }

    ///
    ///return The value including any updates that have happened in this transaction.
    ///
    TA NewValue()
    {
      return !_isValueUpdateSet ? SampleNoTrans() : ValueUpdate;
    }

    ///
    ///Sample the behavior's current value.
    ///
    ///This should generally be avoided in favour of value().listen(..) so you don't
    ///miss any updates, but in many circumstances it makes sense.
    ///
    ///It can be best to use it inside an explicit transaction (using Transaction.run()).
    ///For example, a b.sample() inside an explicit transaction along with a
    ///b.updates().listen(..) will capture the current value and any updates without risk
    ///of missing any in between.
    ///
    public TA Sample()
    {
      return Transaction.Apply(trans => SampleNoTrans());
    }

    public virtual TA SampleNoTrans()
    {
      return EventValue;
    }

    ///
    ///An event that gives the updates for the behavior. If this behavior was created
    ///with a hold, then updates() gives you an event equivalent to the one that was held.
    ///
    public Event<TA> Updates()
    {
      return Event;
    }

    ///
    ///An event that is guaranteed to fire once when you listen to it, giving
    ///the current value of the behavior, and thereafter behaves like updates(),
    ///firing for each update to the behavior's value.
    ///
    public Event<TA> Value()
    {
      return Transaction.Apply(trans => { return Value(trans); });
    }

    Event<TA> Value(Transaction trans1)
    {
      var @out = new EventSink<TA>
      {
        SampleNow = () => new object[] { SampleNoTrans() }
      };
      Listener l = Event.Listen(@out.Node, trans1,
      new TransactionHandler<TA>
      {
        Run = (trans2, a) => @out.Send(trans2, a)
      }, false);
      return @out.AddCleanup(l)
          .LastFiringOnly(trans1);  // Needed in case of an initial value and an update
      // in the same transaction.
    }

    ///
    ///Transform the behavior's value according to the supplied function.
    ///
    public Behavior<TB> Map<TB>(Func<TA, TB> f) 
    {
      return Updates().Map(f).HoldLazy(() => f(SampleNoTrans()));
    }

    ///
    ///Lift a binary function into behaviors.
    ///
    public Behavior<TC> Lift<TB, TC>(Func<TA, TB, TC> f, Behavior<TB> b)
    {
      var ffa = new Func<TA, Func<TB, TC>>(
        aa => (bb => f(aa, bb)));
      Behavior<Func<TB, TC>> bf = Map(ffa);
      return Apply(bf, b);
    }

    ///
    ///Lift a binary function into behaviors.
    ///
    public static Behavior<TC> Lift<TA, TB, TC>(Func<TA, TB, TC> f, Behavior<TA> a, Behavior<TB> b)
    {
      return a.Lift(f, b);
    }

    ///
    ///Lift a ternary function into behaviors.
    ///
    public Behavior<TD> Lift<TB, TC, TD>(Func<TA, TB, TC, TD> f, Behavior<TB> b, Behavior<TC> c)
    {
      var ffa = new Func<TA, Func<TB, Func<TC, TD>>>(aa => (bb => new Func<TC, TD>(cc => f(aa, bb, cc))));
      Behavior<Func<TB, Func<TC, TD>>> bf = Map(ffa);
      return Apply(Apply(bf, b), c);
    }

    ///
    ///Lift a ternary function into behaviors.
    ///
    public static Behavior<TD> Lift<TA, TB, TC, TD>(Func<TA, TB, TC, TD> f, Behavior<TA> a, Behavior<TB> b, Behavior<TC> c)
    {
      return a.Lift(f, b, c);
    }

    ///
    ///Lift a quaternary function into behaviors.
    ///
    public Behavior<TE> Lift<TB,TC,TD,TE>(Func<TA,TB,TC,TD,TE> f, Behavior<TB> b, Behavior<TC> c, Behavior<TD> d)
    {
      var ffa = new Func<TA, Func<TB, Func<TC, Func<TD, TE>>>>(aa => (bb => (cc => new Func<TD,TE>(dd => f(aa, bb, cc, dd)))));
      Behavior<Func<TB, Func<TC, Func<TD, TE>>>> bf = Map(ffa);
      return Apply(Apply(Apply(bf, b), c), d);
    }

    ///
    ///Lift a quaternary function into behaviors.
    ///
    public static Behavior<TE> Lift<TA, TB, TC, TD, TE>(Func<TA, TB, TC, TD, TE> f, Behavior<TA> a, Behavior<TB> b, Behavior<TC> c, Behavior<TD> d)
    {
      return a.Lift(f, b, c, d);
    }

    ///
    ///Apply a value inside a behavior to a function inside a behavior. This is the
    ///primitive for all function lifting.
    ///
    public static Behavior<TB> Apply<TA, TB>(Behavior<Func<TA, TB>> bf, Behavior<TA> ba)
    {
      var @out = new EventSink<TB>();

      var h = new Handler<Transaction>
      {
        Fired = false
      };
      h.Run = trans1 =>
      {
        if (h.Fired)
          return;

        h.Fired = true;
        trans1.Prioritized(
          @out.Node,
          new Handler<Transaction>
          {
            Run = trans2 =>
            {
              @out.Send(trans2, bf.NewValue()(ba.NewValue()));
              h.Fired = false;
            }
          });
      };

      Listener l1 = bf.Updates().Listen_(@out.Node, new TransactionHandler<Func<TA, TB>>
      {
        Run = (trans1, f) => h.Run(trans1)
      });
      Listener l2 = ba.Updates().Listen_(@out.Node, new TransactionHandler<TA>
      {
        Run = (trans1, a) => h.Run(trans1)
      });
      return @out.AddCleanup(l1).AddCleanup(l2).HoldLazy(() => bf.SampleNoTrans()(ba.SampleNoTrans()));
    }

    ///
    ///Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
    ///
    public static Behavior<TA> SwitchB<TA>(Behavior<Behavior<TA>> bba) 
    {
      Func<TA> za = () => bba.SampleNoTrans().SampleNoTrans();
      var @out = new EventSink<TA>();
      var h = new TransactionHandler<Behavior<TA>>();
      h.Run = (trans2, ba) =>
      {
        // Note: If any switch takes place during a transaction, then the
        // value().listen will always cause a sample to be fetched from the
        // one we just switched to. The caller will be fetching our output
        // using value().listen, and value() throws away all firings except
        // for the last one. Therefore, anything from the old input behaviour
        // that might have happened during this transaction will be suppressed.
        if (h.CurrentListener != null)
          h.CurrentListener.Unlisten();
        h.CurrentListener = ba.Value(trans2).Listen(
          @out.Node,
          trans2,
          new TransactionHandler<TA>
          {
            Run = (trans3, a) => @out.Send(trans3, a)
          },
          false);
      };
      Listener l1 = bba.Value().Listen_(@out.Node, h);
      return @out.AddCleanup(l1).HoldLazy(za);
    }

    ///
    ///Unwrap an event inside a behavior to give a time-varying event implementation.
    ///
    public static Event<TA> SwitchE<TA>(Behavior<Event<TA>> bea) 
    {
      return Transaction.Apply(trans => SwitchE(trans, bea));
    }

    private static Event<TA> SwitchE<TA>(Transaction trans1, Behavior<Event<TA>> bea) 
    {
      var @out = new EventSink<TA>();
      var h2 = new TransactionHandler<TA>
      {
        Run = (trans2, a) => @out.Send(trans2, a)
      };
      var h1 = new TransactionHandler<Event<TA>> { CurrentListener = bea.SampleNoTrans().Listen(@out.Node, trans1, h2, false) };
      h1.Run = (trans2, ea) => trans2.Last(
        new Runnable
        {
          Run = () =>
          {
            if (h1.CurrentListener != null)
              h1.CurrentListener.Unlisten();
            h1.CurrentListener = ea.Listen(@out.Node, trans2, h2, true);
          }
        });

      Listener l1 = bea.Updates().Listen(@out.Node, trans1, h1, false);
      return @out.AddCleanup(l1);
    }

    ///
    ///Transform a behavior with a generalized state loop (a mealy machine). The function
    ///is passed the input and the old state and returns the new state and output value.
    ///
    public Behavior<TB> Collect<TB, TS>(TS initState, Func<TA, TS, Tuple<TB, TS>> f)
    {
      return Transaction.Run(() =>
      {
        Event<TA> ea = Updates().Coalesce((fst, snd) => snd);
        Func<Tuple<TB, TS>> zbs = () => f(SampleNoTrans(), initState);
        var ebs = new EventLoop<Tuple<TB, TS>>();
        Behavior<Tuple<TB, TS>> bbs = ebs.HoldLazy(zbs);
        Behavior<TS> bs = bbs.Map((x => x.Item2));
        Event<Tuple<TB, TS>> ebsOut = ea.Snapshot(bs, f);
        ebs.Loop(ebsOut);
        return bbs.Map(x => x.Item1);
      });
    }

    public void Dispose()
    {
      if (cleanup != null)
        cleanup.Unlisten();
    }
  }
}