using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
  public interface IRunnable
  {
    Action Run { get; set; }
  }

  public class Runnable : IRunnable
  {
    public Action Run { get; set; }
  }

  public sealed class Transaction
  {
    // Coarse-grained lock that's held during the whole transaction.
    public static readonly object TransactionLock = new object();
    // Fine-grained lock that protects listeners and nodes.
    public static readonly object ListenersLock = new object();

    // True if we need to re-generate the priority queue.
    public bool ToRegen;

    private class Entry : IComparable<Entry>
    {
      private readonly Node rank;
      public readonly Handler<Transaction> Action;
      private static long nextSeq;
      private readonly long seq;

      public Entry(Node rank, Handler<Transaction> action)
      {
        this.rank = rank;
        Action = action;
        seq = nextSeq++;
      }

      public int CompareTo(Entry other)
      {
        int answer = rank.CompareTo(other.rank);
        if (answer == 0)
        {  // Same rank: preserve chronological sequence.
          if (seq < other.seq) answer = -1;
          else
            if (seq > other.seq) answer = 1;
        }
        return answer;
      }
    }

    //TODO:
    //  private PriorityQueue<Entry> prioritizedQ = new PriorityQueue<Entry>();
    private readonly Queue<Entry> prioritizedQ = new Queue<Entry>();
    private readonly HashSet<Entry> entries = new HashSet<Entry>();
    private readonly List<IRunnable> lastQ = new List<IRunnable>();
    private List<IRunnable> postQ;

    public Transaction()
    {
    }

    private static Transaction currentTransaction;

    /// Return the current transaction, or null if there isn't one.
    public static Transaction GetCurrentTransaction()
    {
      lock (TransactionLock)
      {
        return currentTransaction;
      }
    }

    ///
    ///Run the specified code inside a single transaction.
    ///
    ///In most cases this is not needed, because all APIs will create their own
    ///transaction automatically. It is useful where you want to run multiple
    ///reactive operations atomically.
    ///
    public static void RunVoid(IRunnable code)
    {
      lock (TransactionLock)
      {
        // If we are already inside a transaction (which must be on the same
        // thread otherwise we wouldn't have acquired transactionLock), then
        // keep using that same transaction.
        Transaction transWas = currentTransaction;
        try
        {
          if (currentTransaction == null)
            currentTransaction = new Transaction();
          code.Run();
        }
        finally
        {
          if (transWas == null)
            currentTransaction.Close();
          currentTransaction = transWas;
        }
      }
    }

    public static void RunVoid(Action code)
    {
      RunVoid(new Runnable() { Run = code });
    }

    ///
    ///Run the specified code inside a single transaction, with the contained
    ///code returning a value of the parameter type A.
    ///
    ///In most cases this is not needed, because all APIs will create their own
    ///transaction automatically. It is useful where you want to run multiple
    ///reactive operations atomically.
    ///
    public static A Run<A>(Func<A> code)
    {
      lock (TransactionLock)
      {
        // If we are already inside a transaction (which must be on the same
        // thread otherwise we wouldn't have acquired transactionLock), then
        // keep using that same transaction.
        Transaction transWas = currentTransaction;
        try
        {
          if (currentTransaction == null)
            currentTransaction = new Transaction();
          return code();
        }
        finally
        {
          if (transWas == null && currentTransaction != null)
            currentTransaction.Close();
          currentTransaction = transWas;
        }
      }
    }

    public static void Run(Action<Transaction> code)
    {
      Run(new Handler<Transaction>() { Run = code });
    }

    public static void Run(Handler<Transaction> code)
    {
      lock (TransactionLock)
      {
        // If we are already inside a transaction (which must be on the same
        // thread otherwise we wouldn't have acquired transactionLock), then
        // keep using that same transaction.
        Transaction transWas = currentTransaction;
        try
        {
          if (currentTransaction == null)
            currentTransaction = new Transaction();
          code.Run(currentTransaction);
        }
        finally
        {
          if (transWas == null && currentTransaction != null)
            currentTransaction.Close();
          currentTransaction = transWas;
        }
      }
    }

    public static A Apply<A>(Func<Transaction, A> code)
    {
      lock (TransactionLock)
      {
        // If we are already inside a transaction (which must be on the same
        // thread otherwise we wouldn't have acquired transactionLock), then
        // keep using that same transaction.
        Transaction transWas = currentTransaction;
        try
        {
          if (currentTransaction == null)
            currentTransaction = new Transaction();
          return code(currentTransaction);
        }
        finally
        {
          if (transWas == null && currentTransaction != null)
            currentTransaction.Close();
          currentTransaction = transWas;
        }
      }
    }

    public void Prioritized(Node rank, Action<Transaction> action)
    {
      Prioritized(rank, new Handler<Transaction>() { Run = action });
    }

    public void Prioritized(Node rank, Handler<Transaction> action)
    {
      Entry e = new Entry(rank, action);
      prioritizedQ.Enqueue(e);
      entries.Add(e);
    }

    ///
    ///Add an action to run after all prioritized() actions.
    ///
    public void Last(IRunnable action)
    {
      lastQ.Add(action);
    }

    ///
    /// Add an action to run after all last() actions.
    ///
    public void Post(IRunnable action)
    {
      if (postQ == null)
        postQ = new List<IRunnable>();
      postQ.Add(action);
    }

    ///
    ///If the priority queue has entries in it when we modify any of the nodes'
    ///ranks, then we need to re-generate it to make sure it's up-to-date.
    ///
    private void CheckRegen()
    {
      if (ToRegen)
      {
        ToRegen = false;
        prioritizedQ.Clear();
        foreach (Entry e in entries)
          prioritizedQ.Enqueue(e);
      }
    }

    public void Close()
    {
      while (true)
      {
        CheckRegen();
        if (!prioritizedQ.Any()) break;
        Entry e = prioritizedQ.Dequeue();
        entries.Remove(e);
        e.Action.Run(this);
      }
      foreach (IRunnable action in lastQ)
        action.Run();
      lastQ.Clear();
      if (postQ != null)
      {
        foreach (IRunnable action in postQ)
          action.Run();
        postQ.Clear();
      }
    }

  }

  internal class RunableTask
  {
  }
}
