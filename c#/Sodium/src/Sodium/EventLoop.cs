using System;
using System.Collections.Generic;

namespace Sodium
{

  public class EventLoop<TA> : Event<TA>
  {
    Event<TA> eaOut;

    public EventLoop()
    {
      if (Transaction.GetCurrentTransaction() == null)
        throw new Exception("EventLoop/BehaviorLoop must be used within an explicit transaction");

      SampleNow = () =>
      {
        if (eaOut == null)
          throw new Exception("EventLoop sampled before it was looped");
        return eaOut.SampleNow();
      };
    }


    // TO DO: Copy & paste from EventSink. Can we improve this?
    void Send(Transaction trans, TA a)
    {
      if (Firings.IsEmpty())
        trans.Last(
          new Runnable
          {
            Run = () => Firings.Clear()
          });
      Firings.Add(a);

      var listeners = (List<TransactionHandler<TA>>)Listeners.Clone();
      foreach (TransactionHandler<TA> action in listeners)
      {
        try
        {
          action.Run(trans, a);
        }
        catch (Exception e)
        {
          Console.WriteLine(e);
        }
      }
    }

    public void Loop(Event<TA> eventOut)
    {
      if (this.eaOut != null)
        throw new Exception("EventLoop looped more than once");
      this.eaOut = eventOut;
      EventLoop<TA> me = this;
      AddCleanup(
        eventOut.Listen_(
          Node,
          new TransactionHandler<TA>
          {
            Run = (trans, a) => me.Send(trans, a)
          }));
    }
  }

}