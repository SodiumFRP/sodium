using System;
using System.Collections.Generic;

namespace Sodium.Frp
{
    internal abstract class Node
    {
        public const int NullRank = int.MaxValue;

        // Fine-grained lock that protects listeners and nodes.
        protected static readonly object ListenersLock = new object();

        internal static readonly object NodeRanksLock = new object();

        internal int Rank;

        internal List<TransactionInternal.Entry> Entries = new List<TransactionInternal.Entry>();

        internal Node()
        {
        }

        protected Node(int rank) => this.Rank = rank;

        protected static void EnsureBiggerThan(TransactionInternal trans, Node node, int limit)
        {
            if (node.Rank > limit)
            {
                return;
            }

            node.Rank = limit + 1;

            foreach (TransactionInternal.Entry e in node.Entries)
            {
                trans.AddRerankEntry(e);
            }

            lock (ListenersLock)
            {
                foreach (Target t in node.GetListenerTargetsUnsafe())
                {
                    EnsureBiggerThanRecursive(trans, node, t.Node, node.Rank);
                }
            }
        }

        // ReSharper disable once ParameterOnlyUsedForPreconditionCheck.Local
        private static void EnsureBiggerThanRecursive(TransactionInternal trans, Node originalNode, Node node, int limit)
        {
            if (ReferenceEquals(originalNode, node))
            {
                throw new Exception("A dependency cycle was detected.");
            }

            if (node.Rank > limit)
            {
                return;
            }

            node.Rank = limit + 1;

            foreach (TransactionInternal.Entry e in node.Entries)
            {
                trans.AddRerankEntry(e);
            }

            foreach (Target t in node.GetListenerTargetsUnsafe())
            {
                EnsureBiggerThanRecursive(trans, originalNode, t.Node, node.Rank);
            }
        }

        // Returns the targets themselves rather than projecting out their nodes, so that walking
        // them does not allocate a LINQ iterator per node visited during a rerank cascade.
        protected abstract IReadOnlyList<Target> GetListenerTargetsUnsafe();

        public abstract class Target
        {
            public readonly Node Node;
            public bool IsActivated;

            protected Target(Node node, bool isActivated)
            {
                this.Node = node;
                this.IsActivated = isActivated;
            }
        }
    }

    internal class Node<T> : Node
    {
        public static readonly Node<T> Null = new Node<T>(NullRank);

        private HashSet<Target> listeners = new HashSet<Target>();
        private int listenersCapacity;

        // Snapshot of listeners, rebuilt lazily. Send walks the listener set on every single
        // firing while the set itself only changes when the graph is wired up or a dead weak
        // reference is reaped, so without this every firing allocated a fresh array.
        // Null means stale; all mutations below null it out under ListenersLock.
        private Target[] listenersSnapshot;

        internal Node()
        {
        }

        private Node(int rank)
            : base(rank)
        {
        }

        /// <summary>
        ///     Link an action and a target node to this node.
        /// </summary>
        /// <param name="trans">The current transaction.</param>
        /// <param name="action">The action to link to this node.</param>
        /// <param name="target">The target node to link to this node.</param>
        /// <returns>
        ///     A tuple containing whether changes were made to the node rank
        ///     and the <see cref="Target" /> object created for this link.
        /// </returns>
        internal Target Link(TransactionInternal trans, Action<TransactionInternal, T> action, Node target)
        {
            Target t = new Target(action, target, trans.ActivatedTargets);
            if (!trans.ActivatedTargets)
            {
                trans.AddTargetToActivate(t);
            }
            lock (ListenersLock)
            {
                this.listeners.Add(t);
                this.listenersCapacity++;
                this.listenersSnapshot = null;
            }
            lock (NodeRanksLock)
            {
                EnsureBiggerThan(trans, target, this.Rank);
            }
            return t;
        }

        internal void Unlink(Target target)
        {
            this.RemoveListener(target);
        }

        public new class Target : Node.Target
        {
            public readonly WeakReference<Action<TransactionInternal, T>> Action;

            public Target(Action<TransactionInternal, T> action, Node node, bool isActivated)
                : base(node, isActivated) => this.Action = new WeakReference<Action<TransactionInternal, T>>(action);
        }

        internal IReadOnlyList<Target> GetListenersCopy()
        {
            lock (ListenersLock)
            {
                return this.GetListenersSnapshotUnsafe();
            }
        }

        internal void RemoveListener(Target target)
        {
            lock (ListenersLock)
            {
                this.listeners.Remove(target);
                this.listenersSnapshot = null;
                // HashSet does not reclaim space after items are removed, so we will create a new one if we can reclaim a substantial amount of space
                if (this.listenersCapacity > 100 && this.listeners.Count < this.listenersCapacity / 2)
                {
                    this.listeners = new HashSet<Target>(this.listeners);
                    this.listenersCapacity = this.listeners.Count;
                }
            }
        }

        // Callers must hold ListenersLock. The returned array is never handed out for mutation,
        // and each snapshot is immutable once built, so a caller that is still walking an older
        // one after an invalidation simply sees the listener set as of when it started - exactly
        // the semantics the previous copy-per-call gave.
        private Target[] GetListenersSnapshotUnsafe()
        {
            if (this.listenersSnapshot == null)
            {
                Target[] snapshot = new Target[this.listeners.Count];
                this.listeners.CopyTo(snapshot);
                this.listenersSnapshot = snapshot;
            }

            return this.listenersSnapshot;
        }

        protected override IReadOnlyList<Node.Target> GetListenerTargetsUnsafe() => this.GetListenersSnapshotUnsafe();
    }
}