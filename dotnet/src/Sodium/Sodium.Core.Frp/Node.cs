using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium.Frp
{
    internal abstract class Node
    {
        public static int NullRank = int.MaxValue;

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
                trans.RerankEntriesSet.Add(e);
            }

            lock (ListenersLock)
            {
                foreach (Node n in node.GetListenerNodesUnsafe())
                {
                    EnsureBiggerThanRecursive(trans, node, n, node.Rank);
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
                trans.RerankEntriesSet.Add(e);
            }

            foreach (Node n in node.GetListenerNodesUnsafe())
            {
                EnsureBiggerThanRecursive(trans, originalNode, n, node.Rank);
            }
        }

        protected abstract IEnumerable<Node> GetListenerNodesUnsafe();

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
                trans.TargetsToActivate.Add(t);
            }
            lock (ListenersLock)
            {
                this.listeners.Add(t);
                this.listenersCapacity++;
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
                return this.listeners.ToArray();
            }
        }

        internal void RemoveListener(Target target)
        {
            lock (ListenersLock)
            {
                this.listeners.Remove(target);
                // HashSet does not reclaim space after items are removed, so we will create a new one if we can reclaim a substantial amount of space
                if (this.listenersCapacity > 100 && this.listeners.Count < this.listenersCapacity / 2)
                {
                    this.listeners = new HashSet<Target>(this.listeners);
                    this.listenersCapacity = this.listeners.Count;
                }
            }
        }

        protected override IEnumerable<Node> GetListenerNodesUnsafe()
        {
            return this.listeners.Select(l => l.Node);
        }
    }
}