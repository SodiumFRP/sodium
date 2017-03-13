using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    internal abstract class Node : IComparable<Node>
    {
        // Fine-grained lock that protects listeners and nodes.
        protected static readonly object ListenersLock = new object();

        internal long Rank;

        internal Node()
        {
        }

        protected Node(long rank)
        {
            this.Rank = rank;
        }

        public int CompareTo(Node other)
        {
            return this.Rank.CompareTo(other.Rank);
        }

        internal bool FixRank()
        {
            bool changed = false;
            lock (ListenersLock)
            {
                foreach (Node listener in this.GetListenerNodesUnsafe())
                {
                    if (FixRankRecursive(listener, this.Rank, new HashSet<Node>()))
                    {
                        changed = true;
                    }
                }
            }
            return changed;
        }

        protected static bool FixRankRecursive(Node node, long limit, HashSet<Node> visited)
        {
            if (visited.Contains(node))
            {
                return false;
            }

            bool changed = false;
            if (node.Rank <= limit)
            {
                node.Rank = limit + 1;
                changed = true;
            }

            visited.Add(node);
            foreach (Node n in node.GetListenerNodesUnsafe())
            {
                if (FixRankRecursive(n, node.Rank, visited))
                {
                    changed = true;
                }
            }

            return changed;
        }

        protected static bool EnsureBiggerThan(Node node, long limit, HashSet<Node> visited)
        {
            if (node.Rank > limit || visited.Contains(node))
            {
                return false;
            }

            visited.Add(node);
            node.Rank = limit + 1;
            lock (ListenersLock)
            {
                foreach (Node n in node.GetListenerNodesUnsafe())
                {
                    EnsureBiggerThan(n, node.Rank, visited);
                }
            }

            return true;
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
        public static readonly Node<T> Null = new Node<T>(long.MaxValue);

        private HashSet<Target> listeners = new HashSet<Target>();
        private long listenersCapacity;

        internal Node()
        {
        }

        private Node(long rank)
            : base(rank)
        {
        }

        /// <summary>
        ///     Link an action and a target node to this node.
        /// </summary>
        /// <param name="action">The action to link to this node.</param>
        /// <param name="target">The target node to link to this node.</param>
        /// <returns>
        ///     A tuple containing whether or not changes were made to the node rank
        ///     and the <see cref="Target" /> object created for this link.
        /// </returns>
        internal Tuple<bool, Target> Link(Transaction trans, Action<Transaction, T> action, Node target)
        {
            lock (ListenersLock)
            {
                bool changed = trans.ReachedClose && EnsureBiggerThan(target, this.Rank, new HashSet<Node>());
                Target t = new Target(action, target, !trans.IsConstructing);
                if (trans.IsConstructing)
                {
                    trans.TargetsToActivate.Add(t);
                }
                this.listeners.Add(t);
                this.listenersCapacity++;
                return Tuple.Create(changed, t);
            }
        }

        internal void Unlink(Target target)
        {
            this.RemoveListener(target);
        }

        public new class Target : Node.Target
        {
            public readonly WeakReference<Action<Transaction, T>> Action;

            public Target(Action<Transaction, T> action, Node node, bool isActivated)
                : base(node, isActivated)
            {
                this.Action = new WeakReference<Action<Transaction, T>>(action);
            }
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