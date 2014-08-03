using System;
using System.Collections.Generic;

namespace Sodium
{

  public class EventSink<TA> : Event<TA>
  {
    public EventSink() { }

    public void Send(TA a)
    {
      Transaction.Run(new Handler<Transaction>
      {
        Run = trans => Send(trans, a)
      });
    }

    public void Send(Transaction trans, TA a)
    {
      if (Firings.IsEmpty())
        trans.Last(new Runnable
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
  }
}