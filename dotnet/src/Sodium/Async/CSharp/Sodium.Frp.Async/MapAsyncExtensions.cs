using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using JetBrains.Annotations;
using Sodium.Functional;

namespace Sodium.Frp.Async
{
    /// <summary>
    ///     Extension methods that bridge an impure asynchronous operation into the FRP world:
    ///     listen on a Stream&lt;TInput&gt;, run an async operation per firing, push the result into a
    ///     StreamSink&lt;TResult&gt;, and expose what's queued/running — optionally wired up to streams
    ///     that trigger cancellation of queued or running work alike. The returned
    ///     <see cref="AsyncMapStatus{TInput}" /> is IDisposable; disposing it is how you tear the whole
    ///     pipeline down.
    ///     <para>
    ///         The overloads differ only in how the call's own
    ///         <c>TInput</c>/<c>TResult</c> reach the types the <c>strategy</c> argument is written
    ///         against. Pick by what your strategy needs: one that only schedules (Parallel, Queue,
    ///         SwitchLatest) ignores both, so neither converter is needed; one that inspects the input
    ///         (QueuePerGroup) needs <c>TInput</c> to either already be its input type or be
    ///         convertible to it. Every overload forwards to the last one, which takes both converters
    ///         explicitly and imposes no relationship at all.
    ///     </para>
    /// </summary>
    [PublicAPI]
    public static class AsyncStreamExtensions
    {
        /// <summary>
        ///     For a strategy that only cares about scheduling, not about
        ///     <typeparamref name="TInput" />/<typeparamref name="TResult" /> themselves — Parallel,
        ///     Queue, SwitchLatest. Both are erased to <see cref="Unit" /> before reaching it, so
        ///     neither converter is needed. See the canonical
        ///     <see cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">The type carried by the source stream.</typeparam>
        /// <typeparam name="TResult">The type <paramref name="operation" /> produces on success.</typeparam>
        /// <param name="source">The stream of inputs to run against.</param>
        /// <param name="results">Where each successful run's value is sent, in completion order.</param>
        /// <param name="errors">Where every failed run's exception is sent.</param>
        /// <param name="operation">The asynchronous work to run per input.</param>
        /// <param name="strategy">How overlapping requests are handled.</param>
        /// <param name="cancelAll">Optional. Each firing cancels every tracked operation.</param>
        /// <param name="cancelMatching">Optional. Each firing cancels tracked operations by input value.</param>
        /// <param name="cancelOnDispose">Whether disposing also cancels what's tracked; true by default.</param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" /> reporting what's queued and running; disposing
        ///     it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<Unit, Unit> strategy,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true) =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: _ => Unit.Value,
                resultConverter: _ => Unit.Value,
                cancelAll: ToUnitInternalStream(cancelAll),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     For a strategy that inspects the input but publishes no meaningful result (the result
        ///     is erased to <see cref="Unit" />), where <typeparamref name="TInput" /> already is the
        ///     <typeparamref name="TStrategyInput" /> the strategy is written against — so no
        ///     converter is needed. See the canonical
        ///     <see cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">The type carried by the source stream.</typeparam>
        /// <typeparam name="TResult">The type <paramref name="operation" /> produces on success.</typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Usually just
        ///     <typeparamref name="TInput" />; the <c>where TInput : TStrategyInput</c> constraint
        ///     additionally lets a strategy written against a base class or interface be shared across
        ///     MapAsync calls over several different, more specific <typeparamref name="TInput" />
        ///     types, while the strategy still operates purely in terms of
        ///     <typeparamref name="TStrategyInput" />.
        /// </typeparam>
        /// <param name="source">The stream of inputs to run against.</param>
        /// <param name="results">Where each successful run's value is sent, in completion order.</param>
        /// <param name="errors">Where every failed run's exception is sent.</param>
        /// <param name="operation">The asynchronous work to run per input.</param>
        /// <param name="strategy">How overlapping requests are handled.</param>
        /// <param name="cancelAll">Optional. Each firing cancels every tracked operation.</param>
        /// <param name="cancelMatching">Optional. Each firing cancels tracked operations by input value.</param>
        /// <param name="cancelOnDispose">Whether disposing also cancels what's tracked; true by default.</param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" /> reporting what's queued and running; disposing
        ///     it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, Unit> strategy,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
            where TInput : TStrategyInput =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: v => v,
                resultConverter: _ => Unit.Value,
                cancelAll: ToUnitInternalStream(cancelAll),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     For a strategy that inspects the input but publishes no meaningful result (the result
        ///     is erased to <see cref="Unit" />), where <paramref name="inputConverter" /> derives what
        ///     the strategy sees — QueuePerGroup deriving its group key, say. See the canonical
        ///     <see cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">The type carried by the source stream.</typeparam>
        /// <typeparam name="TResult">The type <paramref name="operation" /> produces on success.</typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TInput" /> by inheritance at all, since
        ///     <paramref name="inputConverter" /> derives it explicitly.
        /// </typeparam>
        /// <param name="source">The stream of inputs to run against.</param>
        /// <param name="results">Where each successful run's value is sent, in completion order.</param>
        /// <param name="errors">Where every failed run's exception is sent.</param>
        /// <param name="operation">The asynchronous work to run per input.</param>
        /// <param name="strategy">How overlapping requests are handled.</param>
        /// <param name="inputConverter">
        ///     Converts each <typeparamref name="TInput" /> value to the
        ///     <typeparamref name="TStrategyInput" /> <paramref name="strategy" /> is written against,
        ///     before it's admitted.
        /// </param>
        /// <param name="cancelAll">Optional. Each firing cancels every tracked operation.</param>
        /// <param name="cancelMatching">Optional. Each firing cancels tracked operations by input value.</param>
        /// <param name="cancelOnDispose">Whether disposing also cancels what's tracked; true by default.</param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" /> reporting what's queued and running; disposing
        ///     it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, Unit> strategy,
            Func<TInput, TStrategyInput> inputConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true) =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: inputConverter,
                resultConverter: _ => Unit.Value,
                cancelAll: ToUnitInternalStream(cancelAll),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     For a strategy that inspects the result but not the input (the input is erased to
        ///     <see cref="Unit" />), where <typeparamref name="TResult" /> already is the
        ///     <typeparamref name="TStrategyResult" /> the strategy is written against — so no
        ///     converter is needed. See the canonical
        ///     <see cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">The type carried by the source stream.</typeparam>
        /// <typeparam name="TResult">The type <paramref name="operation" /> produces on success.</typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Usually just
        ///     <typeparamref name="TResult" />; the <c>where TResult : TStrategyResult</c> constraint
        ///     additionally lets a strategy written against a base class or interface be shared across
        ///     MapAsync calls over several different, more specific <typeparamref name="TResult" />
        ///     types.
        /// </typeparam>
        /// <param name="source">The stream of inputs to run against.</param>
        /// <param name="results">Where each successful run's value is sent, in completion order.</param>
        /// <param name="errors">Where every failed run's exception is sent.</param>
        /// <param name="operation">The asynchronous work to run per input.</param>
        /// <param name="strategy">How overlapping requests are handled.</param>
        /// <param name="cancelAll">Optional. Each firing cancels every tracked operation.</param>
        /// <param name="cancelMatching">Optional. Each firing cancels tracked operations by input value.</param>
        /// <param name="cancelOnDispose">Whether disposing also cancels what's tracked; true by default.</param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" /> reporting what's queued and running; disposing
        ///     it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<Unit, TStrategyResult> strategy,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
            where TResult : TStrategyResult =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: _ => Unit.Value,
                resultConverter: v => v,
                cancelAll: ToUnitInternalStream(cancelAll),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     For a strategy that inspects the result but not the input (the input is erased to
        ///     <see cref="Unit" />), where <paramref name="resultConverter" /> derives what the
        ///     strategy sees. See the canonical
        ///     <see cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">The type carried by the source stream.</typeparam>
        /// <typeparam name="TResult">The type <paramref name="operation" /> produces on success.</typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TResult" /> by inheritance at all, since
        ///     <paramref name="resultConverter" /> derives it explicitly.
        /// </typeparam>
        /// <param name="source">The stream of inputs to run against.</param>
        /// <param name="results">Where each successful run's value is sent, in completion order.</param>
        /// <param name="errors">Where every failed run's exception is sent.</param>
        /// <param name="operation">The asynchronous work to run per input.</param>
        /// <param name="strategy">How overlapping requests are handled.</param>
        /// <param name="resultConverter">
        ///     Converts each successful <typeparamref name="TResult" /> to the
        ///     <typeparamref name="TStrategyResult" /> <paramref name="strategy" /> is written against,
        ///     before <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.OnCompleted" /> sees
        ///     it. Not called for a failed or canceled run, which have no result to convert.
        /// </param>
        /// <param name="cancelAll">Optional. Each firing cancels every tracked operation.</param>
        /// <param name="cancelMatching">Optional. Each firing cancels tracked operations by input value.</param>
        /// <param name="cancelOnDispose">Whether disposing also cancels what's tracked; true by default.</param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" /> reporting what's queued and running; disposing
        ///     it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<Unit, TStrategyResult> strategy,
            Func<TResult, TStrategyResult> resultConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true) =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: _ => Unit.Value,
                resultConverter: resultConverter,
                cancelAll: ToUnitInternalStream(cancelAll),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     For a strategy that inspects both the input and the result, where
        ///     <typeparamref name="TInput" /> and <typeparamref name="TResult" /> already are the
        ///     types it's written against — so neither converter is needed. See the canonical
        ///     <see cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">The type carried by the source stream.</typeparam>
        /// <typeparam name="TResult">The type <paramref name="operation" /> produces on success.</typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Usually just
        ///     <typeparamref name="TInput" />; the <c>where TInput : TStrategyInput</c> constraint
        ///     additionally lets a strategy written against a base class or interface be shared across
        ///     MapAsync calls over several different, more specific <typeparamref name="TInput" />
        ///     types.
        /// </typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against, in the same spirit as
        ///     <typeparamref name="TStrategyInput" /> — inferred, usually just
        ///     <typeparamref name="TResult" />, and related to it by the
        ///     <c>where TResult : TStrategyResult</c> constraint.
        /// </typeparam>
        /// <param name="source">The stream of inputs to run against.</param>
        /// <param name="results">Where each successful run's value is sent, in completion order.</param>
        /// <param name="errors">Where every failed run's exception is sent.</param>
        /// <param name="operation">The asynchronous work to run per input.</param>
        /// <param name="strategy">How overlapping requests are handled.</param>
        /// <param name="cancelAll">Optional. Each firing cancels every tracked operation.</param>
        /// <param name="cancelMatching">Optional. Each firing cancels tracked operations by input value.</param>
        /// <param name="cancelOnDispose">Whether disposing also cancels what's tracked; true by default.</param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" /> reporting what's queued and running; disposing
        ///     it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, TStrategyResult> strategy,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
            where TInput : TStrategyInput
            where TResult : TStrategyResult =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: v => v,
                resultConverter: v => v,
                cancelAll: ToUnitInternalStream(cancelAll),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     For a strategy that inspects both the input and the result, where
        ///     <paramref name="inputConverter" /> derives the input it sees but
        ///     <typeparamref name="TResult" /> already is the
        ///     <typeparamref name="TStrategyResult" />. See the canonical
        ///     <see cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">The type carried by the source stream.</typeparam>
        /// <typeparam name="TResult">The type <paramref name="operation" /> produces on success.</typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TInput" /> by inheritance at all, since
        ///     <paramref name="inputConverter" /> derives it explicitly.
        /// </typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against — inferred, usually just
        ///     <typeparamref name="TResult" />, and related to it by the
        ///     <c>where TResult : TStrategyResult</c> constraint.
        /// </typeparam>
        /// <param name="source">The stream of inputs to run against.</param>
        /// <param name="results">Where each successful run's value is sent, in completion order.</param>
        /// <param name="errors">Where every failed run's exception is sent.</param>
        /// <param name="operation">The asynchronous work to run per input.</param>
        /// <param name="strategy">How overlapping requests are handled.</param>
        /// <param name="inputConverter">
        ///     Converts each <typeparamref name="TInput" /> value to the
        ///     <typeparamref name="TStrategyInput" /> <paramref name="strategy" /> is written against,
        ///     before it's admitted.
        /// </param>
        /// <param name="cancelAll">Optional. Each firing cancels every tracked operation.</param>
        /// <param name="cancelMatching">Optional. Each firing cancels tracked operations by input value.</param>
        /// <param name="cancelOnDispose">Whether disposing also cancels what's tracked; true by default.</param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" /> reporting what's queued and running; disposing
        ///     it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, TStrategyResult> strategy,
            Func<TInput, TStrategyInput> inputConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
            where TResult : TStrategyResult =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: inputConverter,
                resultConverter: v => v,
                cancelAll: ToUnitInternalStream(cancelAll),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     For a strategy that inspects both the input and the result, where
        ///     <typeparamref name="TInput" /> already is the <typeparamref name="TStrategyInput" /> but
        ///     <paramref name="resultConverter" /> derives the result it sees. See the canonical
        ///     <see cref="MapAsync{TInput,TResult,TStrategyInput,TStrategyResult}(Stream{TInput},StreamSink{TResult},StreamSink{Exception},Func{TInput,CancellationToken,Task{TResult}},AsyncConcurrencyStrategyBase{TStrategyInput,TStrategyResult},Func{TInput,TStrategyInput},Func{TResult,TStrategyResult},Stream{Unit},Stream{IReadOnlyCollection{TInput}},bool)" />
        ///     overload for the full parameter contract.
        /// </summary>
        /// <typeparam name="TInput">The type carried by the source stream.</typeparam>
        /// <typeparam name="TResult">The type <paramref name="operation" /> produces on success.</typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred, usually just
        ///     <typeparamref name="TInput" />, and related to it by the
        ///     <c>where TInput : TStrategyInput</c> constraint.
        /// </typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TResult" /> by inheritance at all, since
        ///     <paramref name="resultConverter" /> derives it explicitly.
        /// </typeparam>
        /// <param name="source">The stream of inputs to run against.</param>
        /// <param name="results">Where each successful run's value is sent, in completion order.</param>
        /// <param name="errors">Where every failed run's exception is sent.</param>
        /// <param name="operation">The asynchronous work to run per input.</param>
        /// <param name="strategy">How overlapping requests are handled.</param>
        /// <param name="resultConverter">
        ///     Converts each successful <typeparamref name="TResult" /> to the
        ///     <typeparamref name="TStrategyResult" /> <paramref name="strategy" /> is written against,
        ///     before <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.OnCompleted" /> sees
        ///     it. Not called for a failed or canceled run, which have no result to convert.
        /// </param>
        /// <param name="cancelAll">Optional. Each firing cancels every tracked operation.</param>
        /// <param name="cancelMatching">Optional. Each firing cancels tracked operations by input value.</param>
        /// <param name="cancelOnDispose">Whether disposing also cancels what's tracked; true by default.</param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" /> reporting what's queued and running; disposing
        ///     it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, TStrategyResult> strategy,
            Func<TResult, TStrategyResult> resultConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true)
            where TInput : TStrategyInput =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: v => v,
                resultConverter: resultConverter,
                cancelAll: ToUnitInternalStream(cancelAll),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        /// <summary>
        ///     The fully general form every other MapAsync
        ///     overload eventually forwards to, and the only one that needs no relationship at all
        ///     between <typeparamref name="TInput" />/<typeparamref name="TResult" /> and
        ///     <typeparamref name="TStrategyInput" />/<typeparamref name="TStrategyResult" /> — both
        ///     <paramref name="inputConverter" /> and <paramref name="resultConverter" /> are supplied
        ///     explicitly instead. Prefer one of the narrower overloads where it fits (they exist
        ///     precisely so most calls don't need to write out both converters); reach for this one
        ///     when <paramref name="strategy" /> is written against types unrelated by inheritance to
        ///     this call's own <typeparamref name="TInput" />/<typeparamref name="TResult" /> in both
        ///     directions at once.
        /// </summary>
        /// <typeparam name="TInput">
        ///     The type carried by the source stream — the input each invocation of
        ///     <paramref name="operation" /> receives, and the type reported by
        ///     <see cref="AsyncMapStatus{TInput}.Items" /> and matched against by
        ///     <paramref name="cancelMatching" />.
        /// </typeparam>
        /// <typeparam name="TResult">
        ///     The type <paramref name="operation" /> produces on success, sent to
        ///     <paramref name="results" />.
        /// </typeparam>
        /// <typeparam name="TStrategyInput">
        ///     The input type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TInput" /> by inheritance at all, since
        ///     <paramref name="inputConverter" /> derives it explicitly.
        /// </typeparam>
        /// <typeparam name="TStrategyResult">
        ///     The result type <paramref name="strategy" /> is written against — inferred from
        ///     <paramref name="strategy" />'s type, never specified explicitly. Not required to be
        ///     related to <typeparamref name="TResult" /> by inheritance at all, since
        ///     <paramref name="resultConverter" /> derives it explicitly.
        /// </typeparam>
        /// <param name="source">
        ///     The stream of inputs to run against. Every firing is offered to
        ///     <paramref name="strategy" />, which decides whether it starts immediately or waits.
        ///     After the returned status is disposed, further firings are ignored entirely.
        /// </param>
        /// <param name="results">
        ///     Required. Each successful operation's return value is sent here, in completion order
        ///     rather than the order inputs arrived. A result whose run was superseded or canceled
        ///     is not sent — see <paramref name="strategy" />.
        /// </param>
        /// <param name="errors">
        ///     Required. Every failed operation is sent here — there is deliberately no way to call
        ///     this method without somewhere for errors to go.
        /// </param>
        /// <param name="operation">
        ///     The asynchronous work to run per input. It's invoked inline, so it doesn't reach a
        ///     thread pool until it awaits something itself, and it's handed a CancellationToken
        ///     combining this item's own cancellation with any token the strategy supplied. Honoring
        ///     that token is what makes <paramref name="cancelAll" />, <paramref name="cancelMatching" />,
        ///     and <paramref name="cancelOnDispose" /> take effect on work that has already started —
        ///     an operation that ignores it still runs to completion, and cancellation only means its
        ///     result goes unpublished. There is deliberately no overload that omits the token.
        /// </param>
        /// <param name="strategy">
        ///     How overlapping requests are handled. A strategy instance holds no state of its own
        ///     and may safely be reused across multiple MapAsync calls (even concurrently) — each
        ///     call gets its own freshly created state manager, so separate pipelines never share
        ///     scheduling state.
        /// </param>
        /// <param name="inputConverter">
        ///     Converts each <typeparamref name="TInput" /> value to the
        ///     <typeparamref name="TStrategyInput" /> <paramref name="strategy" /> is written against,
        ///     before it's admitted.
        /// </param>
        /// <param name="resultConverter">
        ///     Converts each successful <typeparamref name="TResult" /> to the
        ///     <typeparamref name="TStrategyResult" /> <paramref name="strategy" /> is written against,
        ///     before <see cref="AsyncConcurrencyStrategy{TInput,TResult,TState}.OnCompleted" /> sees it.
        /// </param>
        /// <param name="cancelAll">
        ///     Optional. Each firing cancels every tracked operation — queued or already running.
        ///     A queued item that's canceled is simply never started when its turn comes.
        ///     Cancellation only takes effect for a running operation if it observes its
        ///     CancellationToken.
        /// </param>
        /// <param name="cancelMatching">
        ///     Optional. Each firing cancels whichever tracked operations (queued or running) were
        ///     admitted for an input value present in the fired collection (compared with the
        ///     default equality comparer for TInput). Same caveats as <paramref name="cancelAll" />.
        /// </param>
        /// <param name="cancelOnDispose">
        ///     Whether disposing the returned <see cref="AsyncMapStatus{TInput}" /> also cancels every
        ///     item tracked at that point (queued or running) — true by default. Either way,
        ///     disposing always, unconditionally, stops any further values from ever being admitted.
        ///     This is fixed here, at setup, rather than being a parameter of Dispose itself, since
        ///     IDisposable.Dispose() is deliberately the only way to dispose.
        ///     Note that disposing never gags the pipeline: whatever is still in flight runs to
        ///     completion and still publishes to <paramref name="results" />/<paramref name="errors" />
        ///     after the call returns. With true that's usually moot, since canceled outcomes are
        ///     never published and an operation that honors its token won't produce one — but an
        ///     operation that ignores its token will, and with false it's the entire point.
        /// </param>
        /// <returns>
        ///     An <see cref="AsyncMapStatus{TInput}" />: IsRunning is a Cell&lt;bool&gt; that is true while
        ///     at least one invocation is actually running (not merely queued), updating glitch-free
        ///     in the same transaction as whichever event caused it to change; Items lists every
        ///     tracked value with its status; disposing it tears the pipeline down.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        ///     <paramref name="source" />, <paramref name="results" />, <paramref name="errors" />,
        ///     <paramref name="operation" />, or <paramref name="strategy" /> is null.
        /// </exception>
        public static AsyncMapStatus<TInput> MapAsync<TInput, TResult, TStrategyInput, TStrategyResult>(
            this Stream<TInput> source,
            StreamSink<TResult> results,
            StreamSink<Exception> errors,
            Func<TInput, CancellationToken, Task<TResult>> operation,
            AsyncConcurrencyStrategyBase<TStrategyInput, TStrategyResult> strategy,
            Func<TInput, TStrategyInput> inputConverter,
            Func<TResult, TStrategyResult> resultConverter,
            Stream<Unit>? cancelAll = null,
            Stream<IReadOnlyCollection<TInput>>? cancelMatching = null,
            bool cancelOnDispose = true) =>
            source.MapAsyncImpl(
                results: results,
                errors: errors,
                operation: operation,
                strategy: strategy,
                inputConverter: inputConverter,
                resultConverter: resultConverter,
                cancelAll: ToUnitInternalStream(cancelAll),
                cancelMatching: cancelMatching,
                cancelOnDispose: cancelOnDispose);

        // Sodium.Core.Frp.Async takes its cancelAll as a Stream<UnitInternal>, since Core has no
        // "don't care" type of its own it could expose. Mapping rather than casting is what keeps
        // UnitInternal — internal to Sodium.Core.Frp, and so unnameable by anyone consuming this
        // library — out of every signature above, leaving Sodium.Functional.Unit as the only unit
        // type a C# caller ever sees. The F# wrapper does the same thing for its own native unit;
        // see toUnitInternalStream there.
        private static Stream<UnitInternal>? ToUnitInternalStream(Stream<Unit>? cancelAll) =>
            cancelAll?.MapImpl(_ => UnitInternal.Value);
    }
}