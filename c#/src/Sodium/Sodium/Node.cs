using System;
using System.Collections.Generic;
using System.Linq;

namespace Sodium
{
    internal abstract class Node : IComparable<Node>
    {
        // Fine-grained lock that protects listeners and nodes.
        protected static readonly object ListenersLock = new object();

        private long rank;

        internal Node(long rank)
        {
            this.rank = rank;
        }

        internal long Rank => this.rank;

        public int CompareTo(Node other)
        {
            return this.rank.CompareTo(other.rank);
        }

        protected static bool EnsureBiggerThan(Node node, long limit, HashSet<Node> visited)
        {
            if (node.rank > limit || visited.Contains(node))
            {
                return false;
            }

            visited.Add(node);
            node.rank = limit + 1;
            foreach (Node n in node.GetListenerNodesUnsafe())
            {
                EnsureBiggerThan(n, node.rank, visited);
            }

            return true;
        }

        protected abstract IEnumerable<Node> GetListenerNodesUnsafe();

        public class Target
        {
            public readonly Node Node;

            public Target(Node node)
            {
                this.Node = node;
            }
        }
    }

    internal class Node<T> : Node
    {
        public static readonly Node<T> Null = new Node<T>(long.MaxValue);

        private readonly List<Target> listeners = new List<Target>();

        internal Node(long rank)
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
        internal Tuple<bool, Target> Link(Action<Transaction, T> action, Node target)
        {
            lock (ListenersLock)
            {
                bool changed = EnsureBiggerThan(target, this.Rank, new HashSet<Node>());
                Target t = new Target(action, target);
                this.listeners.Add(t);
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

            public Target(Action<Transaction, T> action, Node node)
                : base(node)
            {
                this.Action = new WeakReference<Action<Transaction, T>>(action);
            }
        }

        internal IReadOnlyList<Target> GetListeners()
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
            }
        }

        protected override IEnumerable<Node> GetListenerNodesUnsafe()
        {
            lock (ListenersLock)
            {
                return this.listeners.Select(l => l.Node);
            }
        }
    }
}