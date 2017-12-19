using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Sodium
{
    /// <summary>
    ///     Helper methods for creating a <see cref="Stream{T}" />.
    /// </summary>
    public static class Stream
    {
        /// <summary>
        ///     Creates a stream that never fires.
        /// </summary>
        /// <typeparam name="T">The type of the values that would be fired by the stream if it did fire values.</typeparam>
        /// <returns>A stream that never fires.</returns>
        public static Stream<T> Never<T>() => new Stream<T>();

        /// <summary>
        ///     Creates a StreamSink that throws an exception if <see cref="Stream{T}.Send" /> is called more than once per
        ///     transaction.
        /// </summary>
        /// <typeparam name="T">The type of values fired by the stream sink.</typeparam>
        public static StreamSink<T> CreateSink<T>() => new StreamSink<T>();

        /// <summary>
        ///     Construct a StreamSink that uses
        ///     <param name="coalesce" />
        ///     to combine values if <see cref="Stream{T}.Send" /> is called more than once per transaction.
        /// </summary>
        /// <param name="coalesce">
        ///     Function to combine values when <see cref="Stream{T}.Send" /> is called more than once per
        ///     transaction.
        /// </param>
        /// <typeparam name="T">The type of values fired by the stream sink.</typeparam>
        public static StreamSink<T> CreateSink<T>(Func<T, T, T> coalesce) => new StreamSink<T>(coalesce);

        /// <summary>
        ///     Creates a <see cref="StreamLoop{T}" />.  This must be called and looped from within the same transaction.
        /// </summary>
        /// <typeparam name="T">The type of values in the stream loop.</typeparam>
        public static StreamLoop<T> CreateLoop<T>() => new StreamLoop<T>();
    }

    /// <summary>
    ///     Represents a stream of discrete events/firings.
    /// </summary>
    /// <typeparam name="T">The type of values fired by the stream.</typeparam>
    public class Stream<T>
    {
        private readonly Guid streamId;
        internal readonly Node<T> Node;

        // ReSharper disable once CollectionNeverQueried.Local
        private readonly List<IListener> attachedListeners;

        private readonly StreamListenerManager.StreamListeners trackedListeners;
        private readonly List<T> firings;
        internal readonly IKeepListenersAlive KeepListenersAlive;

        internal Stream()
            : this(new KeepListenersAliveImplementation())
        {
        }

        internal Stream(IKeepListenersAlive keepListenersAlive)
        {
            this.streamId = Guid.NewGuid();
            this.KeepListenersAlive = keepListenersAlive;
            this.Node = new Node<T>();
            this.attachedListeners = new List<IListener>();
            this.trackedListeners = new StreamListenerManager.StreamListeners(this.streamId);
            this.firings = new List<T>();
        }

        /// <summary>
        ///     Listen for events/firings on this stream.  The returned <see cref="IListener" /> may be
        ///     disposed to stop listening.  This is an OPERATIONAL mechanism for interfacing between
        ///     the world of I/O and FRP.
        /// </summary>
        /// <param name="handler">The handler to execute for values fired by the stream.</param>
        /// <returns>An <see cref="IListener" /> which may be disposed to stop listening.</returns>
        /// <remarks>
        ///     <para>
        ///         No assumptions should be made about what thread the handler is called on and it should not block.
        ///         Neither <see cref="StreamSink{T}.Send" /> nor <see cref="CellSink{T}.Send" /> may be called from the
        ///         handler.
        ///         They will throw an exception because this method is not meant to be used to create new primitives.
        ///     </para>
        ///     <para>
        ///         If the <see cref="IListener" /> is not disposed, it will continue to listen until this stream is either
        ///         disposed or garbage collected.
        ///     </para>
        ///     <para>
        ///         To ensure this <see cref="IListener" /> is disposed as soon as the stream it is listening to is either
        ///         disposed, pass the returned listener to this stream's <see cref="AttachListener" /> method.
        ///     </para>
        /// </remarks>
        public IStrongListener Listen(Action<T> handler)
        {
            IWeakListener innerListener = this.ListenWeak(handler);
            StrongListener listener = null;
            listener = new StrongListener(
                () =>
                {
                    innerListener.Unlisten();

                    lock (this.KeepListenersAlive)
                    {
                        // ReSharper disable AccessToModifiedClosure
                        if (listener != null)
                        {
                            this.KeepListenersAlive.StopKeepingListenerAlive(listener);
                        }
                        // ReSharper restore AccessToModifiedClosure
                    }
                },
                innerListener);

            lock (this.KeepListenersAlive)
            {
                this.KeepListenersAlive.KeepListenerAlive(listener);
            }

            return listener;
        }

        /// <summary>
        ///     Listen for events/firings on this stream.  The returned <see cref="IListener" /> may be
        ///     disposed to stop listening, or it will automatically stop listening when it is garbage collected.
        ///     This is an OPERATIONAL mechanism for interfacing between the world of I/O and FRP.
        /// </summary>
        /// <param name="handler">The handler to execute for values fired by the stream.</param>
        /// <returns>An <see cref="IListener" /> which may be disposed to stop listening.</returns>
        /// <remarks>
        ///     <para>
        ///         No assumptions should be made about what thread the handler is called on and it should not block.
        ///         Neither <see cref="StreamSink{T}.Send" /> nor <see cref="CellSink{T}.Send" /> may be called from the
        ///         handler.
        ///         They will throw an exception because this method is not meant to be used to create new primitives.
        ///     </para>
        ///     <para>
        ///         If the <see cref="IListener" /> is not disposed, it will continue to listen until this stream is either
        ///         disposed or garbage collected or the listener itself is garbage collected.
        ///     </para>
        ///     <para>
        ///         To ensure this <see cref="IListener" /> is disposed as soon as the stream it is listening to is either
        ///         disposed, pass the returned listener to this stream's <see cref="AttachListener" /> method.
        ///     </para>
        /// </remarks>
        public IWeakListener ListenWeak(Action<T> handler) => this.Listen(Node<T>.Null, (trans2, a) => handler(a));

        /// <summary>
        ///     Attach a listener to this stream so it doesn't get garbage collected until this stream is garbage collected.
        /// </summary>
        /// <param name="listener">The listener to garbage collect along with this stream.</param>
        /// <returns>
        ///     A new stream equivalent to this stream which will garbage collect <paramref name="listener" /> when it is
        ///     garbage collected.
        /// </returns>
        public Stream<T> AttachListener(IListener listener)
        {
            return Transaction.Run(() => this.UnsafeAttachListener(listener));
        }

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of values fired by the stream.</typeparam>
        /// <param name="handler">The handler to execute for values fired by this stream.</param>
        /// <returns></returns>
        public IListener ListenOnce(Action<T> handler)
        {
            IListener listener = null;
            listener = this.Listen(
                a =>
                {
                    // ReSharper disable once AccessToModifiedClosure
                    listener?.Unlisten();

                    handler(a);
                });
            return listener;
        }

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of values fired by the stream.</typeparam>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public TaskWithListener<T> ListenOnceAsync() => this.ListenOnceAsync(CancellationToken.None);

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of values fired by the stream.</typeparam>
        /// <param name="token">The cancellation token.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public TaskWithListener<T> ListenOnceAsync(CancellationToken token)
        {
            return this.ListenOnceAsync(t => t, CancellationToken.None);
        }

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of values fired by the stream.</typeparam>
        /// <param name="modifyTask">A function to modify the task produced by this method.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public TaskWithListener ListenOnceAsync(Func<Task<T>, Task> modifyTask) =>
            this.ListenOnceAsync(modifyTask, CancellationToken.None);

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of values fired by the stream.</typeparam>
        /// <param name="modifyTask">A function to modify the task produced by this method.</param>
        /// <param name="token">The cancellation token.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public TaskWithListener ListenOnceAsync(Func<Task<T>, Task> modifyTask, CancellationToken token)
        {
            return this.ListenOnceAsyncInternal((t, l) => new TaskWithListener(modifyTask(t), l), token);
        }

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of values fired by the stream.</typeparam>
        /// <typeparam name="TResult">The type of the result of the task.</typeparam>
        /// <param name="modifyTask">A function to modify the task produced by this method.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public TaskWithListener<TResult> ListenOnceAsync<TResult>(Func<Task<T>, Task<TResult>> modifyTask) =>
            this.ListenOnceAsync(modifyTask, CancellationToken.None);

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of values fired by the stream.</typeparam>
        /// <typeparam name="TResult">The type of the result of the task.</typeparam>
        /// <param name="modifyTask">A function to modify the task produced by this method.</param>
        /// <param name="token">The cancellation token.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public TaskWithListener<TResult> ListenOnceAsync<TResult>(
            Func<Task<T>, Task<TResult>> modifyTask,
            CancellationToken token)
        {
            return this.ListenOnceAsyncInternal((t, l) => new TaskWithListener<TResult>(modifyTask(t), l), token);
        }

        private TResult ListenOnceAsyncInternal<TResult>(
            Func<Task<T>, IListener, TResult> generateResult,
            CancellationToken token)
        {
            TaskCompletionSource<T> tcs = new TaskCompletionSource<T>();

            IListener listener = null;
            listener = this.Listen(
                a =>
                {
                    // ReSharper disable once AccessToModifiedClosure
                    listener?.Unlisten();

                    tcs.TrySetResult(a);
                });

            token.Register(
                () =>
                {
                    listener?.Unlisten();

                    tcs.TrySetCanceled();
                });

            return generateResult(tcs.Task, listener);
        }

        internal IWeakListener Listen(Node target, Action<Transaction, T> action) => Transaction.Apply(
            trans1 => this.Listen(target, trans1, action, false),
            false);

        internal IWeakListener Listen(
            Node target,
            Transaction trans,
            Action<Transaction, T> action,
            bool suppressEarlierFirings)
        {
            (bool changed, Node<T>.Target nodeTarget) = this.Node.Link(trans, action, target);
            if (changed)
            {
                trans.SetNeedsRegenerating();
            }
            // ReSharper disable once LocalVariableHidesMember
            List<T> firings = this.firings.ToList();
            if (!suppressEarlierFirings && firings.Count > 0)
            {
                trans.Prioritized(
                    target,
                    trans2 =>
                    {
                        // Anything sent already in this transaction must be sent now so that
                        // there's no order dependency between send and listen.
                        foreach (T a in firings)
                        {
                            Transaction.InCallback++;
                            try
                            {
                                // Don't allow transactions to interfere with Sodium
                                // internals.
                                action(trans2, a);
                            }
                            finally
                            {
                                Transaction.InCallback--;
                            }
                        }
                    });
            }
            return new ListenerImplementation(this, action, nodeTarget);
        }

        /// <summary>
        ///     Transform the stream values according to the supplied function, so the returned
        ///     stream's values reflect the value of the function applied to the input stream's values.
        /// </summary>
        /// <typeparam name="TResult">The type of values fired by the returned stream.</typeparam>
        /// <param name="f">
        ///     Function to apply to convert the values.  It may construct FRP logic or use <see cref="Cell{T}.Sample()" />,
        ///     in which case it is equivalent to calling <see cref="Snapshot{TResult}(Cell{TResult})" /> on the cell.
        ///     Other than this, the function must be a pure function.
        /// </param>
        /// <returns>A stream which fires values transformed by <paramref name="f" /> for each value fired by this stream.</returns>
        public Stream<TResult> Map<TResult>(Func<T, TResult> f)
        {
            Stream<TResult> @out = new Stream<TResult>(this.KeepListenersAlive);
            IListener l = this.Listen(@out.Node, (trans2, a) => @out.Send(trans2, f(a)));
            return @out.UnsafeAttachListener(l);
        }

        /// <summary>
        ///     Transform the stream values to the specified constant value.
        /// </summary>
        /// <typeparam name="TResult">The type of the constant value fired by the returned stream.</typeparam>
        /// <param name="value">
        ///     The constant value to return from this mapping.
        /// </param>
        /// <returns>A stream which fires the constant value for each value fired by this stream.</returns>
        public Stream<TResult> MapTo<TResult>(TResult value)
        {
            return this.Map(_ => value);
        }

        /// <summary>
        ///     Create a cell with the specified initial value, that is updated by this stream's values.
        /// </summary>
        /// <param name="initialValue">The initial value of the cell.</param>
        /// <returns>A cell with the specified initial value, that is updated by this stream's values.</returns>
        /// <remarks>
        ///     There is an implicit delay; state updates caused by stream event firings don't become
        ///     visible as the cell's current value as viewed by
        ///     <see cref="Stream{T}.Snapshot{T2, TResult}(Cell{T2}, Func{T, T2, TResult})" />
        ///     until the following transaction. To put this another way,
        ///     <see cref="Stream{T}.Snapshot{T2, TResult}(Cell{T2}, Func{T, T2, TResult})" /> always sees the value of a cell as
        ///     it was before
        ///     any state changes from the current transaction.
        /// </remarks>
        public DiscreteCell<T> Hold(T initialValue) => new DiscreteCell<T>(this.HoldInternal(initialValue));

        internal Cell<T> HoldInternal(T initialValue) => new Cell<T>(this, initialValue);

        /// <summary>
        ///     Create a cell with the specified lazily initialized initial value, that is updated by this stream's values.
        /// </summary>
        /// <param name="initialValue">The lazily initialized initial value of the cell.</param>
        /// <returns>A cell with the specified lazily initialized initial value, that is updated by this stream's values.</returns>
        public DiscreteCell<T> HoldLazy(Lazy<T> initialValue) =>
            new DiscreteCell<T>(this.HoldLazyInternal(initialValue));

        internal Cell<T> HoldLazyInternal(Lazy<T> initialValue) => new LazyCell<T>(this, initialValue);

        /// <summary>
        ///     Return a stream whose events are the values of the cell at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="c">The cell to combine with.</param>
        /// <returns>A stream whose events are the values of the cell at the time of the stream event firing.</returns>
        public Stream<TResult> Snapshot<TResult>(DiscreteCell<TResult> c) => this.Snapshot(c.Cell);

        /// <summary>
        ///     Return a stream whose events are the values of the cell at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="c">The cell to combine with.</param>
        /// <returns>A stream whose events are the values of the cell at the time of the stream event firing.</returns>
        public Stream<TResult> Snapshot<TResult>(Cell<TResult> c)
        {
            return this.Snapshot(c, (a, b) => b);
        }

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cell at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T1">The type of the cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="c">The cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cell at the time of the stream event firing.
        /// </returns>
        public Stream<TResult> Snapshot<T1, TResult>(DiscreteCell<T1> c, Func<T, T1, TResult> f) =>
            this.Snapshot(c.Cell, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cell at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T1">The type of the cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="c">The cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cell at the time of the stream event firing.
        /// </returns>
        public Stream<TResult> Snapshot<T1, TResult>(Cell<T1> c, Func<T, T1, TResult> f)
        {
            Stream<TResult> @out = new Stream<TResult>(this.KeepListenersAlive);
            IListener l = this.Listen(@out.Node, (trans2, a) => @out.Send(trans2, f(a, c.SampleNoTransaction())));
            return @out.UnsafeAttachListener(l);
        }

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cells at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T1">The type of the first cell.</typeparam>
        /// <typeparam name="T2">The type of the second cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="c1">The first cell to combine with.</param>
        /// <param name="c2">The second cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cells at the time of the stream event firing.
        /// </returns>
        public Stream<TResult> Snapshot<T1, T2, TResult>(
            DiscreteCell<T1> c1,
            DiscreteCell<T2> c2,
            Func<T, T1, T2, TResult> f) => this.Snapshot(c1.Cell, c2.Cell, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cells at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T1">The type of the first cell.</typeparam>
        /// <typeparam name="T2">The type of the second cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="c1">The first cell to combine with.</param>
        /// <param name="c2">The second cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cells at the time of the stream event firing.
        /// </returns>
        public Stream<TResult> Snapshot<T1, T2, TResult>(Cell<T1> c1, Cell<T2> c2, Func<T, T1, T2, TResult> f)
        {
            Stream<TResult> @out = new Stream<TResult>(this.KeepListenersAlive);
            IListener l = this.Listen(
                @out.Node,
                (trans2, a) => @out.Send(trans2, f(a, c1.SampleNoTransaction(), c2.SampleNoTransaction())));
            return @out.UnsafeAttachListener(l);
        }

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cells at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T1">The type of the first cell.</typeparam>
        /// <typeparam name="T2">The type of the second cell.</typeparam>
        /// <typeparam name="T3">The type of the third cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="c1">The first cell to combine with.</param>
        /// <param name="c2">The second cell to combine with.</param>
        /// <param name="c3">The third cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cells at the time of the stream event firing.
        /// </returns>
        public Stream<TResult> Snapshot<T1, T2, T3, TResult>(
            DiscreteCell<T1> c1,
            DiscreteCell<T2> c2,
            DiscreteCell<T3> c3,
            Func<T, T1, T2, T3, TResult> f) => this.Snapshot(c1.Cell, c2.Cell, c3.Cell, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cells at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T1">The type of the first cell.</typeparam>
        /// <typeparam name="T2">The type of the second cell.</typeparam>
        /// <typeparam name="T3">The type of the third cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="c1">The first cell to combine with.</param>
        /// <param name="c2">The second cell to combine with.</param>
        /// <param name="c3">The third cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cells at the time of the stream event firing.
        /// </returns>
        public Stream<TResult> Snapshot<T1, T2, T3, TResult>(
            Cell<T1> c1,
            Cell<T2> c2,
            Cell<T3> c3,
            Func<T, T1, T2, T3, TResult> f)
        {
            Stream<TResult> @out = new Stream<TResult>(this.KeepListenersAlive);
            IListener l = this.Listen(
                @out.Node,
                (trans2, a) => @out.Send(
                    trans2,
                    f(a, c1.SampleNoTransaction(), c2.SampleNoTransaction(), c3.SampleNoTransaction())));
            return @out.UnsafeAttachListener(l);
        }

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cells at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T1">The type of the first cell.</typeparam>
        /// <typeparam name="T2">The type of the second cell.</typeparam>
        /// <typeparam name="T3">The type of the third cell.</typeparam>
        /// <typeparam name="T4">The type of the fourth cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="c1">The first cell to combine with.</param>
        /// <param name="c2">The second cell to combine with.</param>
        /// <param name="c3">The third cell to combine with.</param>
        /// <param name="c4">The fourth cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cells at the time of the stream event firing.
        /// </returns>
        public Stream<TResult> Snapshot<T1, T2, T3, T4, TResult>(
            DiscreteCell<T1> c1,
            DiscreteCell<T2> c2,
            DiscreteCell<T3> c3,
            DiscreteCell<T4> c4,
            Func<T, T1, T2, T3, T4, TResult> f) => this.Snapshot(c1.Cell, c2.Cell, c3.Cell, c4.Cell, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cells at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T1">The type of the first cell.</typeparam>
        /// <typeparam name="T2">The type of the second cell.</typeparam>
        /// <typeparam name="T3">The type of the third cell.</typeparam>
        /// <typeparam name="T4">The type of the fourth cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="c1">The first cell to combine with.</param>
        /// <param name="c2">The second cell to combine with.</param>
        /// <param name="c3">The third cell to combine with.</param>
        /// <param name="c4">The fourth cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cells at the time of the stream event firing.
        /// </returns>
        public Stream<TResult> Snapshot<T1, T2, T3, T4, TResult>(
            Cell<T1> c1,
            Cell<T2> c2,
            Cell<T3> c3,
            Cell<T4> c4,
            Func<T, T1, T2, T3, T4, TResult> f)
        {
            Stream<TResult> @out = new Stream<TResult>(this.KeepListenersAlive);
            IListener l = this.Listen(
                @out.Node,
                (trans2, a) => @out.Send(
                    trans2,
                    f(
                        a,
                        c1.SampleNoTransaction(),
                        c2.SampleNoTransaction(),
                        c3.SampleNoTransaction(),
                        c4.SampleNoTransaction())));
            return @out.UnsafeAttachListener(l);
        }

        /// <summary>
        ///     Merges this stream with another stream and drops the other stream's value in the simultaneous case.
        /// </summary>
        /// <param name="s">The stream to merge with.</param>
        /// <returns>
        ///     A stream that is the result of merging this stream with another stream and dropping the other stream's value in
        ///     the simultaneous case.
        /// </returns>
        /// <remarks>
        ///     <para>
        ///         In the case where two stream events are simultaneous (i.e. both
        ///         within the same transaction), the event value from this stream will take precedence, and
        ///         the event value from <paramref name="s" /> will be dropped.
        ///         To specify a custom combining function, use <see cref="Stream{T}.Merge(Stream{T}, Func{T, T, T})" />.
        ///         s1.OrElse(s2) is equivalent to s1.Merge(s2, (l, r) =&gt; l).
        ///     </para>
        ///     <para>
        ///         The name OrElse is used instead of Merge to make it clear that care should be taken because stream events can
        ///         be dropped.
        ///     </para>
        /// </remarks>
        public Stream<T> OrElse(Stream<T> s)
        {
            return this.Merge(s, (left, right) => left);
        }

        private Stream<T> Merge(Transaction trans, Stream<T> s)
        {
            Stream<T> @out = new Stream<T>(this.KeepListenersAlive);
            Node<T> left = new Node<T>();
            Node<T> right = @out.Node;
            (bool changed, Node<T>.Target nodeTarget) = left.Link(trans, (t, v) => { }, right);
            if (changed)
            {
                trans.SetNeedsRegenerating();
            }
            Action<Transaction, T> h = @out.Send;
            IListener l1 = this.Listen(left, h);
            IListener l2 = s.Listen(right, h);
            return @out.UnsafeAttachListener(l1)
                .UnsafeAttachListener(l2)
                .UnsafeAttachListener(Listener.Create(left, nodeTarget));
        }

        /// <summary>
        ///     Merge two streams of the same type into one, so that stream event values on either input appear on the returned
        ///     stream.
        /// </summary>
        /// <param name="s">The stream to merge this stream with.</param>
        /// <param name="f">
        ///     Function to combine the values. It may construct FRP logic or use <see cref="Cell{T}.Sample" />.
        ///     Apart from this the function must be pure.
        /// </param>
        /// <returns>
        ///     A stream which is the combination of event values from this stream and stream
        ///     <param name="s" />
        ///     .
        /// </returns>
        /// <remarks>
        ///     If the events are simultaneous (that is, one event from this stream and one from <paramref name="s" />
        ///     occurring in the same transaction), combine them into one using the specified combining function
        ///     so that the returned stream is guaranteed only ever to have one event per transaction.
        ///     The event from this stream will appear at the left input of the combining function, and
        ///     the event from <paramref name="s" /> will appear at the right.
        /// </remarks>
        public Stream<T> Merge(Stream<T> s, Func<T, T, T> f)
        {
            return Transaction.Apply(trans => this.Merge(trans, s, f), false);
        }

        internal Stream<T> Merge(Transaction trans, Stream<T> s, Func<T, T, T> f) =>
            this.Merge(trans, s).Coalesce(trans, f);

        internal Stream<T> Coalesce(Transaction trans1, Func<T, T, T> f)
        {
            Stream<T> @out = new Stream<T>(this.KeepListenersAlive);
            Action<Transaction, T> h = CoalesceHandler.Create(f, @out);
            IListener l = this.Listen(@out.Node, trans1, h, false);
            return @out.UnsafeAttachListener(l);
        }

        /// <summary>
        ///     Clean up the output by discarding any firing other than the last one.
        /// </summary>
        /// <param name="trans">The transaction to get the last firing from.</param>
        /// <returns>A stream containing only the last event firing from the specified transaction.</returns>
        internal Stream<T> LastFiringOnly(Transaction trans)
        {
            return this.Coalesce(trans, (first, second) => second);
        }

        /// <summary>
        ///     Return a stream that only outputs events for which the predicate returns <code>true</code>.
        /// </summary>
        /// <param name="predicate">The predicate used to filter the stream.</param>
        /// <returns>A stream that only outputs events for which the predicate returns <code>true</code>.</returns>
        public Stream<T> Filter(Func<T, bool> predicate)
        {
            Stream<T> @out = new Stream<T>(this.KeepListenersAlive);
            IListener l = this.Listen(
                @out.Node,
                (trans2, a) =>
                {
                    if (predicate(a))
                    {
                        @out.Send(trans2, a);
                    }
                });
            return @out.UnsafeAttachListener(l);
        }

        /// <summary>
        ///     Return a stream that only outputs events from the input stream when the specified cell's value is <code>true</code>
        ///     .
        /// </summary>
        /// <param name="c">The cell that acts as a gate.</param>
        /// <returns>A stream that only outputs events from the input stream when the specified cell's value is <code>true</code>.</returns>
        public Stream<T> Gate(DiscreteCell<bool> c) => this.Gate(c.Cell);

        /// <summary>
        ///     Return a stream that only outputs events from the input stream when the specified cell's value is <code>true</code>
        ///     .
        /// </summary>
        /// <param name="c">The cell that acts as a gate.</param>
        /// <returns>A stream that only outputs events from the input stream when the specified cell's value is <code>true</code>.</returns>
        public Stream<T> Gate(Cell<bool> c)
        {
            return this.Snapshot(c, (a, pred) => pred ? Maybe.Some(a) : Maybe.None).FilterMaybe();
        }

        /// <summary>
        ///     Return a stream that only outputs events which have a different value than the previous event.
        /// </summary>
        /// <returns>A stream that only outputs events which have a different value than the previous event.</returns>
        public Stream<T> Calm() => this.Calm(EqualityComparer<T>.Default);

        /// <summary>
        ///     Return a stream that only outputs events which have a different value than the previous event.
        /// </summary>
        /// <param name="comparer">The equality comparer to use to determine if two items are equal.</param>
        /// <returns>A stream that only outputs events which have a different value than the previous event.</returns>
        public Stream<T> Calm(IEqualityComparer<T> comparer)
        {
            return this.Calm(new Lazy<Maybe<T>>(() => Maybe.None), comparer);
        }

        internal Stream<T> Calm(Lazy<Maybe<T>> init, IEqualityComparer<T> comparer)
        {
            return this.CollectLazy(
                    init,
                    (a, lastA) =>
                    {
                        if (lastA.Match(v => comparer.Equals(v, a), () => false))
                        {
                            return (ReturnValue: Maybe.None, State: lastA);
                        }

                        Maybe<T> ma = Maybe.Some(a);
                        return (ReturnValue: ma, State: ma);
                    })
                .FilterMaybe();
        }

        /// <summary>
        ///     Transform a stream with a generalized state loop (a Mealy machine).
        ///     The function is passed the input and the old state and returns the new state and output value.
        /// </summary>
        /// <typeparam name="TState">The type of the state of the Mealy machine.</typeparam>
        /// <typeparam name="TReturn">The type of the return value.</typeparam>
        /// <param name="initialState">The initial state of the Mealy machine.</param>
        /// <param name="f">
        ///     Function to apply to update the state.  It may construct FRP logic or use
        ///     <see cref="Cell{T}.Sample" />, in which case it is equivalent to snapshotting the cell with
        ///     <see cref="Snapshot{TReturn}(Cell{TReturn})" />.  Apart from this, the function must be pure.
        /// </param>
        /// <returns>A stream resulting from the transformation of this stream by the Mealy machine.</returns>
        public Stream<TReturn> Collect<TState, TReturn>(
            TState initialState,
            Func<T, TState, (TReturn ReturnValue, TState State)> f) =>
            this.CollectLazy(new Lazy<TState>(() => initialState), f);

        /// <summary>
        ///     Transform a stream with a generalized state loop (a Mealy machine) using a lazily evaluated initial state.
        ///     The function is passed the input and the old state and returns the new state and output value.
        /// </summary>
        /// <typeparam name="TState">The type of the state of the Mealy machine.</typeparam>
        /// <typeparam name="TReturn">The type of the return value.</typeparam>
        /// <param name="initialState">The lazily evaluated initial state of the Mealy machine.</param>
        /// <param name="f">
        ///     Function to apply to update the state.  It may construct FRP logic or use
        ///     <see cref="Cell{T}.Sample" />, in which case it is equivalent to snapshotting the cell with
        ///     <see cref="Snapshot{TReturn}(Cell{TReturn})" />.  Apart from this, the function must be pure.
        /// </param>
        /// <returns>A stream resulting from the transformation of this stream by the Mealy machine.</returns>
        public Stream<TReturn> CollectLazy<TState, TReturn>(
            Lazy<TState> initialState,
            Func<T, TState, (TReturn ReturnValue, TState State)> f)
        {
            return Transaction.Run(
                () =>
                {
                    StreamLoop<TState> es = new StreamLoop<TState>();
                    Cell<TState> s = es.HoldLazyInternal(initialState);
                    Stream<(TReturn ReturnValue, TState State)> ebs = this.Snapshot(s, f);
                    Stream<TReturn> eb = ebs.Map(bs => bs.ReturnValue);
                    Stream<TState> esOut = ebs.Map(bs => bs.State);
                    es.Loop(esOut);
                    return eb;
                });
        }

        /// <summary>
        ///     Accumulate on this stream, outputting the new state each time an event fires.
        /// </summary>
        /// <typeparam name="TReturn">The type of the accumulated state.</typeparam>
        /// <param name="initialState">The initial state.</param>
        /// <param name="f">
        ///     Function to apply to update the state.  It may construct FRP logic or use
        ///     <see cref="Cell{T}.Sample" />, in which case it is equivalent to snapshotting the cell with
        ///     <see cref="Snapshot{TReturn}(Cell{TReturn})" />.  Apart from this, the function must be pure.
        /// </param>
        /// <returns>A cell holding the accumulated state of this stream.</returns>
        public DiscreteCell<TReturn> Accum<TReturn>(TReturn initialState, Func<T, TReturn, TReturn> f) =>
            this.AccumLazy(new Lazy<TReturn>(() => initialState), f);

        public DiscreteCell<TReturn> AccumLazy<TReturn>(Lazy<TReturn> initialState, Func<T, TReturn, TReturn> f)
        {
            return Transaction.Run(
                () =>
                {
                    StreamLoop<TReturn> es = new StreamLoop<TReturn>();
                    Cell<TReturn> s = es.HoldLazyInternal(initialState);
                    Stream<TReturn> esOut = this.Snapshot(s, f);
                    es.Loop(esOut);
                    return esOut.HoldLazy(initialState);
                });
        }

        /// <summary>
        ///     Return a stream that outputs only one value: the next event of the input stream starting from the transaction in
        ///     which this method was invoked.
        /// </summary>
        /// <returns>
        ///     A stream that outputs only one value: the next event of the input stream starting from the transaction in
        ///     which this method was invoked.
        /// </returns>
        public Stream<T> Once()
        {
            // This is a bit long-winded but it's efficient because it unregisters
            // the listener.
            Stream<T> @out = new Stream<T>(this.KeepListenersAlive);
            IListener l = null;
            l = this.Listen(
                @out.Node,
                (trans, a) =>
                {
                    // ReSharper disable AccessToModifiedClosure
                    if (l != null)
                    {
                        @out.Send(trans, a);

                        l?.Unlisten();

                        l = null;
                    }
                    // ReSharper restore AccessToModifiedClosure
                });
            return @out.UnsafeAttachListener(l);
        }

        // This is not thread-safe, so one of these two conditions must apply:
        // 1. We are within a transaction, since in the current implementation
        //    a transaction locks out all other threads.
        // 2. The object on which this is being called was created has not yet
        //    been returned from the method where it was created, so it can't
        //    be shared between threads.
        internal Stream<T> UnsafeAttachListener(IListener cleanup)
        {
            this.attachedListeners.Add(cleanup);
            this.trackedListeners.AddListener(cleanup.GetListenerWithWeakReference());
            return this;
        }

        internal void Send(Transaction trans, T a)
        {
            if (this.firings.Count < 1)
            {
                trans.Last(this.firings.Clear);
            }
            this.firings.Add(a);

            foreach (Node<T>.Target target in this.Node.GetListenersCopy())
            {
                trans.Prioritized(
                    target.Node,
                    trans2 =>
                    {
                        Transaction.InCallback++;
                        try
                        {
                            // Don't allow transactions to interfere with Sodium
                            // internals.
                            // Dereference the weak reference
                            if (target.Action.TryGetTarget(out Action<Transaction, T> action))
                            {
                                // If it hasn't been garbage collected, call it.
                                if (target.IsActivated)
                                {
                                    action(trans2, a);
                                }
                            }
                            else
                            {
                                // If it has been garbage collected, remove it.
                                this.Node.RemoveListener(target);
                            }
                        }
                        finally
                        {
                            Transaction.InCallback--;
                        }
                    });
            }
        }

        ~Stream()
        {
            StreamListenerManager.Remove(this.streamId);
        }

        private class StrongListener : IStrongListener
        {
            private readonly Action unlisten;
            private readonly IListener listener;

            public StrongListener(Action unlisten, IListener listener)
            {
                this.unlisten = unlisten;
                this.listener = listener;
            }

            public void Unlisten()
            {
                this.unlisten();
            }

            public IListenerWithWeakReference GetListenerWithWeakReference() =>
                this.listener.GetListenerWithWeakReference();

            public void Dispose()
            {
                this.Unlisten();
            }
        }

        private class ListenerImplementation : IWeakListener
        {
            // It's essential that we keep the action alive, since the node uses
            // a weak reference.
            // ReSharper disable once NotAccessedField.Local
            private readonly Action<Transaction, T> action;

            // It's essential that we keep the listener alive while the caller holds
            // the Listener, so that the garbage collector doesn't get triggered.
            // ReSharper disable once NotAccessedField.Local
            private readonly Stream<T> stream;

            private readonly WeakListener weakListener;

            public ListenerImplementation(Stream<T> stream, Action<Transaction, T> action, Node<T>.Target target)
            {
                this.stream = stream;
                this.action = action;

                this.weakListener = new WeakListener(stream?.Node, target);
            }

            public void Unlisten()
            {
                this.weakListener.Unlisten();
            }

            public IListenerWithWeakReference GetListenerWithWeakReference() => this.weakListener;
        }

        private class WeakListener : IListenerWithWeakReference
        {
            private readonly Node<T> node;
            private readonly Node<T>.Target target;

            public WeakListener(Node<T> node, Node<T>.Target target)
            {
                this.node = node;
                this.target = target;
            }

            public void Unlisten()
            {
                this.node?.Unlink(this.target);
            }
        }

        private class KeepListenersAliveImplementation : IKeepListenersAlive
        {
            private readonly HashSet<IListener> listeners = new HashSet<IListener>();

            // ReSharper disable once CollectionNeverQueried.Local
            private readonly List<IKeepListenersAlive> childKeepListenersAliveList = new List<IKeepListenersAlive>();

            public void KeepListenerAlive(IListener listener)
            {
                this.listeners.Add(listener);
            }

            public void StopKeepingListenerAlive(IListener listener)
            {
                this.listeners.Remove(listener);
            }

            public void Use(IKeepListenersAlive childKeepListenersAlive)
            {
                this.childKeepListenersAliveList.Add(childKeepListenersAlive);
            }
        }
    }
}