using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Sodium.Functional;

namespace Sodium.Frp
{
    public static class StreamExtensionMethods
    {
        /// <summary>
        ///     Listen for events/firings on this stream.  The returned <see cref="IStrongListener" /> may be
        ///     disposed to stop listening.  This is an OPERATIONAL mechanism for interfacing between
        ///     the world of I/O and FRP.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="handler">The handler to execute for values fired by the stream.</param>
        /// <returns>An <see cref="IStrongListener" /> which may be disposed to stop listening.</returns>
        /// <remarks>
        ///     <para>
        ///         No assumptions should be made about what thread the handler is called on and it should not block.
        ///         Neither <see cref="StreamSink{T}.Send" /> nor <see cref="CellSinkExtensionMethods.Send{T}" /> may be called from the
        ///         handler.
        ///         They will throw an exception because this method is not meant to be used to create new primitives.
        ///     </para>
        ///     <para>
        ///         If the <see cref="IStrongListener" /> is not disposed, it will continue to listen until this stream is either
        ///         disposed or garbage collected.
        ///     </para>
        ///     <para>
        ///         To ensure this <see cref="IStrongListener" /> is disposed as soon as the stream it is listening to is either
        ///         disposed, pass the returned listener to this stream's <see cref="AttachListener{T}" /> method.
        ///     </para>
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IStrongListener Listen<T>(this Stream<T> s, Action<T> handler) => s.ListenImpl(handler);

        /// <summary>
        ///     Listen for events/firings on this stream.  The returned <see cref="IWeakListener" /> may be
        ///     disposed to stop listening, or it will automatically stop listening when it is garbage collected.
        ///     This is an OPERATIONAL mechanism for interfacing between the world of I/O and FRP.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="handler">The handler to execute for values fired by the stream.</param>
        /// <returns>An <see cref="IWeakListener" /> which may be disposed to stop listening.</returns>
        /// <remarks>
        ///     <para>
        ///         No assumptions should be made about what thread the handler is called on and it should not block.
        ///         Neither <see cref="StreamSink{T}.Send" /> nor <see cref="CellSinkExtensionMethods.Send{T}" /> may be called from the
        ///         handler.
        ///         They will throw an exception because this method is not meant to be used to create new primitives.
        ///     </para>
        ///     <para>
        ///         If the <see cref="IWeakListener" /> is not disposed, it will continue to listen until this stream is either
        ///         disposed or garbage collected or the listener itself is garbage collected.
        ///     </para>
        ///     <para>
        ///         To ensure this <see cref="IWeakListener" /> is disposed as soon as the stream it is listening to is either
        ///         disposed, pass the returned listener to this stream's <see cref="AttachListener{T}" /> method.
        ///     </para>
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IWeakListener ListenWeak<T>(this Stream<T> s, Action<T> handler) => s.ListenWeakImpl(handler);

        /// <summary>
        ///     Attach a listener to this stream so it doesn't get garbage collected until this stream is garbage collected.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="listener">The listener to garbage collect along with this stream.</param>
        /// <returns>
        ///     A new stream equivalent to this stream which will garbage collect <paramref name="listener" /> when it is
        ///     garbage collected.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> AttachListener<T>(this Stream<T> s, IListener listener) =>
            s.AttachListenerImpl(listener);

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="handler">The handler to execute for values fired by this stream.</param>
        /// <returns></returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static IStrongListener ListenOnce<T>(this Stream<T> s, Action<T> handler) => s.ListenOnceImpl(handler);

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public static TaskWithListener<T> ListenOnceAsync<T>(this Stream<T> s) => s.ListenOnceAsync(CancellationToken.None);

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="token">The cancellation token.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public static TaskWithListener<T> ListenOnceAsync<T>(this Stream<T> s, CancellationToken token) =>
            s.ListenOnceAsync(t => t, CancellationToken.None);

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="modifyTask">A function to modify the task produced by this method.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public static TaskWithListener ListenOnceAsync<T>(this Stream<T> s, Func<Task<T>, Task> modifyTask) =>
            s.ListenOnceAsync(modifyTask, CancellationToken.None);

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="modifyTask">A function to modify the task produced by this method.</param>
        /// <param name="token">The cancellation token.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public static TaskWithListener ListenOnceAsync<T>(this Stream<T> s, Func<Task<T>, Task> modifyTask, CancellationToken token) =>
            s.ListenOnceAsyncInternal((t, l) => new TaskWithListener(modifyTask(t), l), token);

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="TResult">The type of the result of the task.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="modifyTask">A function to modify the task produced by this method.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public static TaskWithListener<TResult> ListenOnceAsync<T, TResult>(this Stream<T> s, Func<Task<T>, Task<TResult>> modifyTask) =>
            s.ListenOnceAsync(modifyTask, CancellationToken.None);

        /// <summary>
        ///     Handle the first event on this stream and then automatically unregister.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="TResult">The type of the result of the task.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="modifyTask">A function to modify the task produced by this method.</param>
        /// <param name="token">The cancellation token.</param>
        /// <returns>A task which completes when a value is fired by this stream.</returns>
        public static TaskWithListener<TResult> ListenOnceAsync<T, TResult>(
            this Stream<T> s,
            Func<Task<T>, Task<TResult>> modifyTask,
            CancellationToken token) =>
            s.ListenOnceAsyncInternal((t, l) => new TaskWithListener<TResult>(modifyTask(t), l), token);

        private static TResult ListenOnceAsyncInternal<T, TResult>(
            this Stream<T> s,
            Func<Task<T>, IStrongListener, TResult> generateResult,
            CancellationToken token)
        {
            TaskCompletionSource<T> tcs = new TaskCompletionSource<T>();

            IStrongListener listener = null;
            bool unlistenEarly = false;
            listener = s.Listen(
                a =>
                {
                    // ReSharper disable once AccessToModifiedClosure
                    if (listener == null)
                    {
                        unlistenEarly = true;
                    }
                    else
                    {
                        // ReSharper disable once AccessToModifiedClosure
                        listener.Unlisten();
                        listener = null;
                    }

                    tcs.TrySetResult(a);
                });
            if (unlistenEarly)
            {
                listener.Unlisten();
                listener = null;
            }

            token.Register(
                () =>
                {
                    listener?.Unlisten();

                    tcs.TrySetCanceled();
                });

            return generateResult(tcs.Task, listener);
        }

        /// <summary>
        ///     Transform the stream values according to the supplied function, so the returned
        ///     stream's values reflect the value of the function applied to the input stream's values.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="TResult">The type of values fired by the returned stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="f">
        ///     Function to apply to convert the values.  It may construct FRP logic or use <see cref="CellExtensionMethods.Sample{T}(Cell{T})" />,
        ///     in which case it is equivalent to calling <see cref="Snapshot{T, TResult}(Stream{T}, Cell{TResult})" /> on the cell.
        ///     Other than this, the function must be a pure function.
        /// </param>
        /// <returns>A stream which fires values transformed by <paramref name="f" /> for each value fired by this stream.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Map<T, TResult>(this Stream<T> s, Func<T, TResult> f) => s.MapImpl(f);

        /// <summary>
        ///     Transform the stream values to the specified constant value.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="TResult">The type of the constant value fired by the returned stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="value">
        ///     The constant value to return from this mapping.
        /// </param>
        /// <returns>A stream which fires the constant value for each value fired by this stream.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> MapTo<T, TResult>(this Stream<T> s, TResult value) => s.MapToImpl(value);

        /// <summary>
        ///     Create a cell with the specified initial value, that is updated by this stream's values.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="initialValue">The initial value of the cell.</param>
        /// <returns>A cell with the specified initial value, that is updated by this stream's values.</returns>
        /// <remarks>
        ///     There is an implicit delay; state updates caused by stream event firings don't become
        ///     visible as the cell's current value as viewed by
        ///     <see cref="StreamExtensionMethods.Snapshot{T, T2, TResult}(Stream{T}, Cell{T2}, Func{T, T2, TResult})" />
        ///     until the following transaction. To put this another way,
        ///     <see cref="StreamExtensionMethods.Snapshot{T, T2, TResult}(Stream{T}, Cell{T2}, Func{T, T2, TResult})" /> always sees the value of a cell as
        ///     it was before
        ///     any state changes from the current transaction.
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<T> Hold<T>(this Stream<T> s, T initialValue) => s.HoldImpl(initialValue);

        /// <summary>
        ///     Create a cell with the specified lazily initialized initial value, that is updated by this stream's values.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="initialValue">The lazily initialized initial value of the cell.</param>
        /// <returns>A cell with the specified lazily initialized initial value, that is updated by this stream's values.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<T> HoldLazy<T>(this Stream<T> s, Lazy<T> initialValue) => s.HoldLazyImpl(initialValue);

        /// <summary>
        ///     Return a stream whose events are the values of the cell at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="c">The cell to combine with.</param>
        /// <returns>A stream whose events are the values of the cell at the time of the stream event firing.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Snapshot<T, TResult>(this Stream<T> s, Cell<TResult> c) => s.SnapshotImpl(c);

        /// <summary>
        ///     Return a stream whose events are the values of the behavior at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="b">The behavior to combine with.</param>
        /// <returns>A stream whose events are the values of the behavior at the time of the stream event firing.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Snapshot<T, TResult>(this Stream<T> s, Behavior<TResult> b) => s.SnapshotImpl(b);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cell at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="T1">The type of the cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="c">The cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cell at the time of the stream event firing.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Snapshot<T, T1, TResult>(this Stream<T> s, Cell<T1> c, Func<T, T1, TResult> f) =>
            s.SnapshotImpl(c, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the behavior at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="T1">The type of the behavior.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="b">The behavior to combine with.</param>
        /// <param name="f">A function to convert the stream value and behavior value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the behavior at the time of the stream event firing.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Snapshot<T, T1, TResult>(
            this Stream<T> s,
            Behavior<T1> b,
            Func<T, T1, TResult> f) => s.SnapshotImpl(b, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cells at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="T1">The type of the first cell.</typeparam>
        /// <typeparam name="T2">The type of the second cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="c1">The first cell to combine with.</param>
        /// <param name="c2">The second cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cells at the time of the stream event firing.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Snapshot<T, T1, T2, TResult>(
            this Stream<T> s,
            Cell<T1> c1,
            Cell<T2> c2,
            Func<T, T1, T2, TResult> f) => s.SnapshotImpl(c1, c2, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the behaviors at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="T1">The type of the first behavior.</typeparam>
        /// <typeparam name="T2">The type of the second behavior.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="b1">The first behavior to combine with.</param>
        /// <param name="b2">The second behavior to combine with.</param>
        /// <param name="f">A function to convert the stream value and behavior value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the behaviors at the time of the stream event firing.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Snapshot<T, T1, T2, TResult>(
            this Stream<T> s,
            Behavior<T1> b1,
            Behavior<T2> b2,
            Func<T, T1, T2, TResult> f) => s.SnapshotImpl(b1, b2, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cells at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="T1">The type of the first cell.</typeparam>
        /// <typeparam name="T2">The type of the second cell.</typeparam>
        /// <typeparam name="T3">The type of the third cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="c1">The first cell to combine with.</param>
        /// <param name="c2">The second cell to combine with.</param>
        /// <param name="c3">The third cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cells at the time of the stream event firing.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Snapshot<T, T1, T2, T3, TResult>(
            this Stream<T> s,
            Cell<T1> c1,
            Cell<T2> c2,
            Cell<T3> c3,
            Func<T, T1, T2, T3, TResult> f) => s.SnapshotImpl(c1, c2, c3, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the behaviors at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="T1">The type of the first behavior.</typeparam>
        /// <typeparam name="T2">The type of the second behavior.</typeparam>
        /// <typeparam name="T3">The type of the third behavior.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="b1">The first behavior to combine with.</param>
        /// <param name="b2">The second behavior to combine with.</param>
        /// <param name="b3">The third behavior to combine with.</param>
        /// <param name="f">A function to convert the stream value and behavior value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the behaviors at the time of the stream event firing.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Snapshot<T, T1, T2, T3, TResult>(
            this Stream<T> s,
            Behavior<T1> b1,
            Behavior<T2> b2,
            Behavior<T3> b3,
            Func<T, T1, T2, T3, TResult> f) => s.SnapshotImpl(b1, b2, b3, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the cells at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="T1">The type of the first cell.</typeparam>
        /// <typeparam name="T2">The type of the second cell.</typeparam>
        /// <typeparam name="T3">The type of the third cell.</typeparam>
        /// <typeparam name="T4">The type of the fourth cell.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="c1">The first cell to combine with.</param>
        /// <param name="c2">The second cell to combine with.</param>
        /// <param name="c3">The third cell to combine with.</param>
        /// <param name="c4">The fourth cell to combine with.</param>
        /// <param name="f">A function to convert the stream value and cell value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the cells at the time of the stream event firing.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Snapshot<T, T1, T2, T3, T4, TResult>(
            this Stream<T> s,
            Cell<T1> c1,
            Cell<T2> c2,
            Cell<T3> c3,
            Cell<T4> c4,
            Func<T, T1, T2, T3, T4, TResult> f) => s.SnapshotImpl(c1, c2, c3, c4, f);

        /// <summary>
        ///     Return a stream whose events are the result of the combination using the specified
        ///     function of the input stream's value and the value of the behaviors at the time of the stream event firing.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="T1">The type of the first behavior.</typeparam>
        /// <typeparam name="T2">The type of the second behavior.</typeparam>
        /// <typeparam name="T3">The type of the third behavior.</typeparam>
        /// <typeparam name="T4">The type of the fourth behavior.</typeparam>
        /// <typeparam name="TResult">The return type.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="b1">The first behavior to combine with.</param>
        /// <param name="b2">The second behavior to combine with.</param>
        /// <param name="b3">The third behavior to combine with.</param>
        /// <param name="b4">The fourth behavior to combine with.</param>
        /// <param name="f">A function to convert the stream value and behavior value into a return value.</param>
        /// <returns>
        ///     A stream whose events are the result of the combination using the specified function of the input stream's
        ///     value and the value of the behaviors at the time of the stream event firing.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TResult> Snapshot<T, T1, T2, T3, T4, TResult>(
            this Stream<T> s,
            Behavior<T1> b1,
            Behavior<T2> b2,
            Behavior<T3> b3,
            Behavior<T4> b4,
            Func<T, T1, T2, T3, T4, TResult> f) => s.SnapshotImpl(b1, b2, b3, b4, f);

        /// <summary>
        ///     Merges this stream with another stream and drops the other stream's value in the simultaneous case.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="s2">The stream to merge with.</param>
        /// <returns>
        ///     A stream that is the result of merging this stream with another stream and dropping the other stream's value in
        ///     the simultaneous case.
        /// </returns>
        /// <remarks>
        ///     <para>
        ///         In the case where two stream events are simultaneous (i.e. both
        ///         within the same transaction), the event value from this stream will take precedence, and
        ///         the event value from <paramref name="s2" /> will be dropped.
        ///         To specify a custom combining function, use <see cref="StreamExtensionMethods.Merge{T}(Stream{T}, Stream{T}, Func{T, T, T})" />.
        ///         s1.OrElse(s2) is equivalent to s1.Merge(s2, (l, r) =&gt; l).
        ///     </para>
        ///     <para>
        ///         The name OrElse is used instead of Merge to make it clear that care should be taken because stream events can
        ///         be dropped.
        ///     </para>
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> OrElse<T>(this Stream<T> s, Stream<T> s2) => s.OrElseImpl(s2);

        /// <summary>
        ///     Merge two streams of the same type into one, so that stream event values on either input appear on the returned
        ///     stream.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="s2">The stream to merge this stream with.</param>
        /// <param name="f">
        ///     Function to combine the values. It may construct FRP logic or use <see cref="CellExtensionMethods.Sample{T}" />.
        ///     Apart from this the function must be pure.
        /// </param>
        /// <returns>
        ///     A stream which is the combination of event values from this stream and stream
        ///     <param name="s2" />
        ///     .
        /// </returns>
        /// <remarks>
        ///     If the events are simultaneous (that is, one event from this stream and one from <paramref name="s2" />
        ///     occurring in the same transaction), combine them into one using the specified combining function
        ///     so that the returned stream is guaranteed only ever to have one event per transaction.
        ///     The event from this stream will appear at the left input of the combining function, and
        ///     the event from <paramref name="s2" /> will appear at the right.
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Merge<T>(this Stream<T> s, Stream<T> s2, Func<T, T, T> f) => s.MergeImpl(s2, f);

        /// <summary>
        ///     Return a stream that only outputs events for which the predicate returns <code>true</code>.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="predicate">The predicate used to filter the stream.</param>
        /// <returns>A stream that only outputs events for which the predicate returns <code>true</code>.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Filter<T>(this Stream<T> s, Func<T, bool> predicate) => s.FilterImpl(predicate);

        /// <summary>
        ///     Return a stream that only outputs events from the input stream when the specified cell's value is <code>true</code>
        ///     .
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="c">The cell that acts as a gate.</param>
        /// <returns>A stream that only outputs events from the input stream when the specified cell's value is <code>true</code>.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Gate<T>(this Stream<T> s, Cell<bool> c) => s.GateImpl(c);

        /// <summary>
        ///     Return a stream that only outputs events from the input stream when the specified behavior's value is <code>true</code>
        ///     .
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="b">The behavior that acts as a gate.</param>
        /// <returns>A stream that only outputs events from the input stream when the specified behavior's value is <code>true</code>.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Gate<T>(this Stream<T> s, Behavior<bool> b) => s.GateImpl(b);

        /// <summary>
        ///     Return a stream that only outputs events which have a different value than the previous event.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <returns>A stream that only outputs events which have a different value than the previous event.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Calm<T>(this Stream<T> s) => s.CalmImpl(EqualityComparer<T>.Default.Equals);

        /// <summary>
        ///     Return a stream that only outputs events which have a different value than the previous event.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="comparer">The equality comparer to use to determine if two items are equal.</param>
        /// <returns>A stream that only outputs events which have a different value than the previous event.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Calm<T>(this Stream<T> s, IEqualityComparer<T> comparer) => s.CalmImpl(comparer.Equals);

        /// <summary>
        ///     Return a stream that only outputs events which have a different value than the previous event.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="areEqual">The function to use to determine if two items are equal.</param>
        /// <returns>A stream that only outputs events which have a different value than the previous event.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Calm<T>(this Stream<T> s, Func<T, T, bool> areEqual) => s.CalmImpl(areEqual);

        /// <summary>
        ///     Transform a stream with a generalized state loop (a Mealy machine).
        ///     The function is passed the input and the old state and returns the new state and output value.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="TState">The type of the state of the Mealy machine.</typeparam>
        /// <typeparam name="TReturn">The type of the return value.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="initialState">The initial state of the Mealy machine.</param>
        /// <param name="f">
        ///     Function to apply to update the state.  It may construct FRP logic or use
        ///     <see cref="CellExtensionMethods.Sample{T}" />, in which case it is equivalent to snapshotting the cell with
        ///     <see cref="Snapshot{T, TReturn}(Stream{T}, Cell{TReturn})" />.  Apart from this, the function must be pure.
        /// </param>
        /// <returns>A stream resulting from the transformation of this stream by the Mealy machine.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TReturn> Collect<T, TState, TReturn>(
            this Stream<T> s,
            TState initialState,
            Func<T, TState, (TReturn ReturnValue, TState State)> f) => s.CollectImpl(initialState, f);

        /// <summary>
        ///     Transform a stream with a generalized state loop (a Mealy machine) using a lazily evaluated initial state.
        ///     The function is passed the input and the old state and returns the new state and output value.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="TState">The type of the state of the Mealy machine.</typeparam>
        /// <typeparam name="TReturn">The type of the return value.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="initialState">The lazily evaluated initial state of the Mealy machine.</param>
        /// <param name="f">
        ///     Function to apply to update the state.  It may construct FRP logic or use
        ///     <see cref="CellExtensionMethods.Sample{T}" />, in which case it is equivalent to snapshotting the cell with
        ///     <see cref="Snapshot{T, TReturn}(Stream{T}, Cell{TReturn})" />.  Apart from this, the function must be pure.
        /// </param>
        /// <returns>A stream resulting from the transformation of this stream by the Mealy machine.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<TReturn> CollectLazy<T, TState, TReturn>(
            this Stream<T> s,
            Lazy<TState> initialState,
            Func<T, TState, (TReturn ReturnValue, TState State)> f) => s.CollectLazyImpl(initialState, f);

        /// <summary>
        ///     Accumulate on this stream, outputting the new state each time an event fires.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="TReturn">The type of the accumulated state.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="initialState">The initial state.</param>
        /// <param name="f">
        ///     Function to apply to update the state.  It may construct FRP logic or use
        ///     <see cref="CellExtensionMethods.Sample{T}" />, in which case it is equivalent to snapshotting the cell with
        ///     <see cref="Snapshot{T, TReturn}(Stream{T}, Cell{TReturn})" />.  Apart from this, the function must be pure.
        /// </param>
        /// <returns>A cell holding the accumulated state of this stream.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TReturn> Accum<T, TReturn>(
            this Stream<T> s,
            TReturn initialState,
            Func<T, TReturn, TReturn> f) => s.AccumImpl(initialState, f);

        /// <summary>
        ///     Accumulate on this stream, outputting the new state each time an event fires using a lazily evaluated initial state.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <typeparam name="TReturn">The type of the accumulated state.</typeparam>
        /// <param name="s">The stream.</param>
        /// <param name="initialState">The lazily evaluated initial state.</param>
        /// <param name="f">
        ///     Function to apply to update the state.  It may construct FRP logic or use
        ///     <see cref="CellExtensionMethods.Sample{T}" />, in which case it is equivalent to snapshotting the cell with
        ///     <see cref="Snapshot{T, TReturn}(Stream{T}, Cell{TReturn})" />.  Apart from this, the function must be pure.
        /// </param>
        /// <returns>A cell holding the accumulated state of this stream.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Cell<TReturn> AccumLazy<T, TReturn>(
            this Stream<T> s,
            Lazy<TReturn> initialState,
            Func<T, TReturn, TReturn> f) => s.AccumLazyImpl(initialState, f);

        /// <summary>
        ///     Return a stream that outputs only one value: the next event of the input stream starting from the transaction in
        ///     which this method was invoked.
        /// </summary>
        /// <typeparam name="T">The type of the stream.</typeparam>
        /// <param name="s">The stream.</param>
        /// <returns>
        ///     A stream that outputs only one value: the next event of the input stream starting from the transaction in
        ///     which this method was invoked.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Once<T>(this Stream<T> s) => s.OnceImpl();

        /// <summary>
        ///     Merges a collection of streams and drops the stream's value specified earlier in the collection in the simultaneous
        ///     case.
        /// </summary>
        /// <param name="s">The collection of streams to merge.</param>
        /// <returns>
        ///     A stream that is the result of merging the collection of streams and dropping the stream's value specified
        ///     earlier in the collection in the simultaneous case.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> OrElse<T>(this IEnumerable<Stream<T>> s) => s.OrElseImpl<T, Stream<T>>();

        /// <summary>
        ///     Merge a collection of streams of the same type into one, so that events on any input appear on the returned stream.
        /// </summary>
        /// <param name="s">The collection of streams to merge.</param>
        /// <param name="f">
        ///     Function to combine the values. It may construct FRP logic or use <see cref="CellExtensionMethods.Sample{T}" />.  Apart
        ///     from this the function must be pure.
        /// </param>
        /// <returns>
        ///     A stream which is the combination of event values from the collection of streams
        ///     <param name="s" />
        ///     .
        /// </returns>
        /// <remarks>
        ///     If the events are simultaneous (that is, one event from more than one stream
        ///     occurring in the same transaction), combine them into one using the specified combining function
        ///     so that the returned stream is guaranteed only ever to have one event per transaction.
        ///     The event from the stream earlier in the collection will appear at the left input of the combining function, and
        ///     the event from the stream later in the collection will appear at the right.
        /// </remarks>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> Merge<T>(this IEnumerable<Stream<T>> s, Func<T, T, T> f) => s.MergeImpl(f);

        /// <summary>
        ///     Return a stream that only outputs events that have values, removing the <see cref="Maybe{T}" /> wrapper, and
        ///     discarding <see cref="Maybe.None" /> values.
        /// </summary>
        /// <param name="s">The stream of <see cref="Maybe{T}" /> values to filter.</param>
        /// <returns>
        ///     A stream that only outputs events that have values, removing the <see cref="Maybe{T}" /> wrapper, and
        ///     discarding <see cref="Maybe.None" /> values.
        /// </returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static Stream<T> FilterMaybe<T>(this Stream<Maybe<T>> s) =>
            s.FilterMaybeImpl<T, Maybe<T>>((m, a) => m.MatchSome(a));
    }
}