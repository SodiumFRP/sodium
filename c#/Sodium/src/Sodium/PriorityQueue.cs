//**********************************************************
//* PriorityQueue                                          *
//* Copyright (c) Julian M Bucknall 2004                   *
//* All rights reserved.                                   *
//* This code can be used in your applications, providing  *
//*    that this copyright comment box remains as-is       *
//**********************************************************
//* .NET priority queue class (heap algorithm)             *
//**********************************************************

// Modified by Michael Lund to handle genererics for use with Sodium.

using System;
using System.Collections;
using System.Collections.Generic;
using System.Security.Permissions;
using System.Runtime.Serialization;

namespace JMBucknall.Containers
{

  [Serializable]
  public struct HeapEntry<TA> 
  {
    private TA item;
    private IComparable priority;
    bool itemSet;

    public HeapEntry(TA item, IComparable priority)
    {
      this.item = item;
      this.priority = priority;
      this.itemSet = true;
    }
    public TA Item
    {
      get
      {
        if(itemSet == false)
          throw new Exception("Item not set");  
        return item;
      }
    }
    public IComparable Priority
    {
      get { return priority; }
    }
    public void Clear()
    {
      itemSet = false;
      priority = null;
    }
  }

  [Serializable]
  public class PriorityQueue<TA> : ICollection, ISerializable where TA : IComparable<TA>
  {
    private int count;
    private int capacity;
    private int version;
    private HeapEntry<TA>[] heap;

    private const string capacityName = "capacity";
    private const string countName = "count";
    private const string heapName = "heap";

    public PriorityQueue()
    {
      capacity = 15; // 15 is equal to 4 complete levels
      heap = new HeapEntry<TA>[capacity];
    }

    protected PriorityQueue(SerializationInfo info, StreamingContext context)
    {
      capacity = info.GetInt32(capacityName);
      count = info.GetInt32(countName);
      var heapCopy = (HeapEntry<TA>[])info.GetValue(heapName, typeof(HeapEntry<TA>[]));
      heap = new HeapEntry<TA>[capacity];
      Array.Copy(heapCopy, 0, heap, 0, count);
      version = 0;
    }

    public TA Dequeue()
    {
      if (count == 0)
        throw new InvalidOperationException();

      TA result = heap[0].Item;
      count--;
      TrickleDown(0, heap[count]);
      heap[count].Clear();
      version++;
      return result;
    }

    public void Enqueue(TA item, IComparable priority)
    {
      if (priority == null)
        throw new ArgumentNullException("priority");
      if (count == capacity)
        GrowHeap();
      count++;
      BubbleUp(count - 1, new HeapEntry<TA>(item, priority));
      version++;
    }

    private void BubbleUp(int index, HeapEntry<TA> he)
    {
      int parent = GetParent(index);
      // note: (index > 0) means there is a parent
      while ((index > 0) &&
            (heap[parent].Priority.CompareTo(he.Priority) < 0))
      {
        heap[index] = heap[parent];
        index = parent;
        parent = GetParent(index);
      }
      heap[index] = he;
    }

    private static int GetLeftChild(int index)
    {
      return (index * 2) + 1;
    }

    private static int GetParent(int index)
    {
      return (index - 1) / 2;
    }

    private void GrowHeap()
    {
      capacity = (capacity * 2) + 1;
      var newHeap = new HeapEntry<TA>[capacity];
      Array.Copy(heap, 0, newHeap, 0, count);
      heap = newHeap;
    }

    private void TrickleDown(int index, HeapEntry<TA> he)
    {
      int child = GetLeftChild(index);
      while (child < count)
      {
        if (((child + 1) < count) &&
            (heap[child].Priority.CompareTo(heap[child + 1].Priority) < 0))
        {
          child++;
        }
        heap[index] = heap[child];
        index = child;
        child = GetLeftChild(index);
      }
      BubbleUp(index, he);
    }

    #region IEnumerable implementation

    //IEnumerator<TA> IEnumerable<TA>.GetEnumerator()
    //{
    //  return new PriorityQueueEnumerator<TA>(this);
    //}

    public IEnumerator GetEnumerator()
    {
      return new PriorityQueueEnumerator(this);
    }
    #endregion

    #region ICollection implementation
    public int Count
    {
      get { return count; }
    }

    public void CopyTo(Array array, int index)
    {
      Array.Copy(heap, 0, array, index, count);
    }

    public object SyncRoot
    {
      get { return this; }
    }

    public bool IsSynchronized
    {
      get { return false; }
    }
    #endregion

    #region ISerializable implementation
    [SecurityPermissionAttribute(SecurityAction.Demand, SerializationFormatter = true)]
    void ISerializable.GetObjectData(SerializationInfo info, StreamingContext context)
    {
      info.AddValue(capacityName, capacity);
      info.AddValue(countName, count);
      var heapCopy = new HeapEntry<TA>[count];
      Array.Copy(heap, 0, heapCopy, 0, count);
      info.AddValue(heapName, heapCopy, typeof(HeapEntry<TA>[]));
    }
    #endregion

    #region Priority Queue enumerator
    [Serializable]
    private class PriorityQueueEnumerator : IEnumerator<TA> 
    {
      private int index;
      private PriorityQueue<TA> pq;
      private int version;

      public PriorityQueueEnumerator(PriorityQueue<TA> pq)
      {
        this.pq = pq;
        Reset();
      }

      private void checkVersion()
      {
        if (version != pq.version)
          throw new InvalidOperationException();
      }

      #region IEnumerator Members

      public void Reset()
      {
        index = -1;
        version = pq.version;
      }

      object IEnumerator.Current
      {
        get { return Current; }
      }

      public TA Current
      {
        get
        {
          checkVersion();
          return pq.heap[index].Item;
        }
      }

      public bool MoveNext()
      {
        checkVersion();
        if (index + 1 == pq.count)
          return false;
        index++;
        return true;
      }

      #endregion

      public void Dispose()
      {
      }
    }
    #endregion

    public void Clear()
    {
      capacity = 15;
      heap = new HeapEntry<TA>[capacity];
      count = 0;
      version = 0;
    }
  }
}
