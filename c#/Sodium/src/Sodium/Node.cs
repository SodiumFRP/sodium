using System;
using System.Collections.Generic;

namespace Sodium
{
  public class Node : IComparable<Node>
  {
    private long rank;
    public static readonly Node Null = new Node(long.MaxValue);

    public Node(long rank)
    {
      this.rank = rank;
    }
    public int CompareTo(Node other)
    {
      if (rank < other.rank) return -1;
      if (rank > other.rank) return 1;
      return 0;
    }

    private readonly HashSet<Node> listeners = new HashSet<Node>();

    public bool LinkTo(Node target)
    {
      if (target == Null)
        return false;
      var changed = target.EnsureBiggerThan(rank, new HashSet<Node>());
      listeners.Add(target);
      return changed;
    }

    public void UnlinkTo(Node target)
    {
      if (target == Null)
        return;
      listeners.Remove(target);
    }

    bool EnsureBiggerThan(long limit, HashSet<Node> visited)
    {
      if (rank > limit || visited.Contains(this))
        return false;
      visited.Add(this);
      rank = limit + 1;
      foreach (var listener in listeners)
      {
        listener.EnsureBiggerThan(rank, visited);
      }
      return true;
    }
  }
}
