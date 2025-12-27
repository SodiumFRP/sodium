using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sodium.Frp
{
    internal class EntryPriorityQueue
    {
        private const bool SanityChecks = false;

        private class HeadAndTail
        {
            public TransactionInternal.Entry Head;
            public TransactionInternal.Entry Tail;

            public HeadAndTail(TransactionInternal.Entry head, TransactionInternal.Entry tail)
            {
                this.Head = head;
                this.Tail = tail;
            }
        }

        private HeadAndTail[] entries = new HeadAndTail[1000];

        private int entriesSize = 1000;

        private HeadAndTail last = new HeadAndTail(null, null);

        private int minRank = 0;
        private int maxRank = -1;

        private void CheckQueue()
        {
            for (int i = 0; i < this.entries.Length; i++)
            {
                HeadAndTail e = this.entries[i];
                if (e == null)
                {
                    continue;
                }

                int expectedPqRank = i;

                if (e.Head != null && e.Head.PqPrev != null)
                {
                    throw new Exception("Head cannot have a previous entry.");
                }

                if (e.Tail != null && e.Tail.PqNext != null)
                {
                    throw new Exception("Tail cannot have a next entry.");
                }

                TransactionInternal.Entry current = e.Head;
                while (current != null)
                {
                    if (current.PqRank != expectedPqRank)
                    {
                        throw new Exception("Rank was not the expected value.");
                    }

                    if (!current.InPq)
                    {
                        throw new Exception("Entry was not marked as being in a priority queue.");
                    }

                    current = current.PqNext;
                }
            }
        }

        internal bool IsEmpty()
        {
            if (this.minRank <= this.maxRank)
            {
                return false;
            }

            this.minRank = 0;
            this.maxRank = -1;
            return this.last.Head == null;
        }

        internal void Enqueue(TransactionInternal.Entry e)
        {
            if (e.PqRank != e.Node.Rank)
            {
                throw new Exception("Enqueue requires ranks to agree.");
            }

            if (e.PqRank != Node.NullRank && e.PqRank > this.entriesSize)
            {
                int newSize = this.entriesSize * 2;
                Array.Resize(ref this.entries, newSize);
                this.entriesSize = newSize;
            }

            if (e.InPq)
            {
                return;
            }

            e.InPq = true;

            HeadAndTail entry;

            if (e.PqRank == Node.NullRank)
            {
                entry = this.last;
            }
            else
            {
                if (e.PqRank < this.minRank)
                {
                    this.minRank = e.PqRank;
                }

                if (e.PqRank > this.maxRank)
                {
                    this.maxRank = e.PqRank;
                }

                entry = this.entries[e.PqRank];

                if (entry == null)
                {
                    this.entries[e.PqRank] = new HeadAndTail(e, e);

                    if (SanityChecks)
                    {
                        this.CheckQueue();
                    }

                    return;
                }
            }

            if (entry.Head == null)
            {
                entry.Head = entry.Tail = e;

                if (SanityChecks)
                {
                    this.CheckQueue();
                }

                return;
            }

            entry.Tail.PqNext = e;
            e.PqPrev = entry.Tail;
            entry.Tail = e;

            if (SanityChecks)
            {
                this.CheckQueue();
            }
        }

        public TransactionInternal.Entry Dequeue()
        {
            while (true)
            {
                if (this.minRank > this.maxRank)
                {
                    this.minRank = 0;
                    this.maxRank = -1;

                    if (this.last.Head != null)
                    {
                        TransactionInternal.Entry result1 = this.last.Head;
                        this.Remove(result1);
                        return result1;
                    }

                    if (SanityChecks)
                    {
                        this.CheckQueue();
                    }

                    return null;
                }

                HeadAndTail entry = this.entries[this.minRank];

                if (entry?.Head == null)
                {
                    this.minRank++;
                    continue;
                }

                TransactionInternal.Entry result = entry.Head;
                this.Remove(result);

                while (true)
                {
                    entry = this.entries[this.minRank];

                    if (entry?.Head == null)
                    {
                        this.minRank++;

                        if (this.minRank > this.maxRank)
                        {
                            this.minRank = 0;
                            this.maxRank = -1;
                            break;
                        }

                        continue;
                    }

                    break;
                }

                if (SanityChecks)
                {
                    // sanity check, find it there is something else with a smaller rank
                    foreach (HeadAndTail entry2 in this.entries)
                    {
                        if (entry2 != null)
                        {
                            TransactionInternal.Entry current = entry2.Head;

                            while (current != null)
                            {
                                if (current.PqRank < result.PqRank)
                                {
                                    throw new Exception("Priority queue contains less than the expected number of elements.");
                                }

                                current = current.PqNext;
                            }
                        }
                    }

                    this.CheckQueue();
                }

                return result;
            }
        }

        private void Remove(TransactionInternal.Entry e)
        {
            if (!e.InPq)
            {
                return;
            }

            e.InPq = false;

            HeadAndTail entry = e.PqRank == Node.NullRank ? this.last : this.entries[e.PqRank];

            if (e.PqPrev != null)
            {
                e.PqPrev.PqNext = e.PqNext;
            }

            if (e.PqNext != null)
            {
                e.PqNext.PqPrev = e.PqPrev;
            }

            if (entry.Head == e)
            {
                entry.Head = entry.Head.PqNext;
            }

            if (entry.Tail == e)
            {
                entry.Tail = entry.Tail.PqPrev;
            }

            if (entry.Head == null)
            {
                entry.Tail = null;
            }

            e.PqNext = null;
            e.PqPrev = null;

            if (SanityChecks)
            {
                this.CheckQueue();

                TransactionInternal.Entry current = entry.Head;

                while (current != null)
                {
                    if (!current.InPq)
                    {
                        throw new Exception("Entry was expected to be in the priority queue.");
                    }

                    current = current.PqNext;
                }
            }
        }

        public void ChangeRank(TransactionInternal.Entry e, int newRank)
        {
            this.Remove(e);
            e.PqRank = newRank;
            this.Enqueue(e);
        }
    }
}