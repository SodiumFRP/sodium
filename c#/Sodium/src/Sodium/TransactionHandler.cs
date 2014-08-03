using System;

namespace Sodium
{
  public class TransactionHandler<TA> : ICloneable, IDisposable
  {
    public Action<Transaction, TA> Run { get; set; }

    public object Clone()
    {
      return new TransactionHandler<TA> { Run = Run };
    }

    public Listener CurrentListener { get; set; }
    public void Dispose()
    {
      if (CurrentListener != null)
        CurrentListener.Unlisten();
    }
  }
}