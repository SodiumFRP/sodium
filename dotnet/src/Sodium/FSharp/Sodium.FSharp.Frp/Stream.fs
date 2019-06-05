module Sodium.Frp.Stream

open System
open System.Threading.Tasks
open System.Collections.Generic
open System.Runtime.CompilerServices

[<MethodImpl(MethodImplOptions.NoInlining)>]
let never<'a> () = StreamInternal.NeverImpl<'a> ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let loop f =
    TransactionInternal.Apply
        (
            (fun transaction _ ->
                let l = LoopedStream ()
                let struct (s, r) = f l
                l.Loop (transaction, s)
                struct (s, r)),
            false
        )

let loopWithNoCaptures f =
    let struct (l, _) = loop (fun s -> struct (f s, ()))
    l

[<MethodImpl(MethodImplOptions.NoInlining)>]
let listenWeak handler (stream : Stream<_>) = stream.ListenWeakImpl (Action<_> handler)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let listen handler (stream : Stream<_>) = stream.ListenImpl (Action<_> handler)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let attachListener listener (stream : Stream<_>) = stream.AttachListenerImpl listener

[<MethodImpl(MethodImplOptions.NoInlining)>]
let listenOnce handler (stream : Stream<_>) = stream.ListenOnceImpl (Action<_> handler)

let listenOnceAsync stream =
    let tcs = TaskCompletionSource<_> ()
    let mutable listenerOption = None
    let mutable unlistenEarly = false
    let listener = stream |> listen (fun a ->
        match listenerOption with
            | None -> unlistenEarly <- true
            | Some listener ->
                listener |> Listener.unlisten
                listenerOption <- None
        tcs.TrySetResult(a) |> ignore)
    listenerOption <- Some listener
    if unlistenEarly then
        listener |> Listener.unlisten
        listenerOption <- None
    async {
        let! ct = Async.CancellationToken
        ct.Register (fun () ->
            Listener.unlisten listener
            tcs.TrySetCanceled () |> ignore) |> ignore
        return! Async.AwaitTask tcs.Task }

[<MethodImpl(MethodImplOptions.NoInlining)>]
let map f (stream : Stream<_>) = stream.MapImpl (Func<_,_> f)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let mapTo value (stream : Stream<_>) = stream.MapToImpl value

[<MethodImpl(MethodImplOptions.NoInlining)>]
let hold initialValue (stream : Stream<_>) = stream.HoldImpl initialValue

[<MethodImpl(MethodImplOptions.NoInlining)>]
let holdLazy initialValue (stream : Stream<_>) = stream.HoldLazyImpl initialValue

[<MethodImpl(MethodImplOptions.NoInlining)>]
let snapshotB (behavior : Behavior<_>) f (stream : Stream<_>) = stream.SnapshotImpl (behavior, (Func<_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let snapshot (cell : Cell<_>) f (stream : Stream<_>) = stream.SnapshotImpl (cell, (Func<_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let snapshotAndTakeB (behavior : Behavior<_>) (stream : Stream<_>) = stream.SnapshotImpl behavior

[<MethodImpl(MethodImplOptions.NoInlining)>]
let snapshotAndTake (cell : Cell<_>) (stream : Stream<_>) = stream.SnapshotImpl cell

[<MethodImpl(MethodImplOptions.NoInlining)>]
let snapshot2B (behavior1 : Behavior<_>) behavior2 f (stream : Stream<_>) = stream.SnapshotImpl (behavior1, behavior2, (Func<_,_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let snapshot2 (cell1 : Cell<_>) cell2 f (stream : Stream<_>) = stream.SnapshotImpl (cell1, cell2, (Func<_,_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let snapshot3B (behavior1 : Behavior<_>) behavior2 behavior3 f (stream : Stream<_>) = stream.SnapshotImpl (behavior1, behavior2, behavior3, (Func<_,_,_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let snapshot3 (cell1 : Cell<_>) cell2 cell3 f (stream : Stream<_>) = stream.SnapshotImpl (cell1, cell2, cell3, (Func<_,_,_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let snapshot4B (behavior1 : Behavior<_>) behavior2 behavior3 behavior4 f (stream : Stream<_>) = stream.SnapshotImpl (behavior1, behavior2, behavior3, behavior4, (Func<_,_,_,_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let snapshot4 (cell1 : Cell<_>) cell2 cell3 cell4 f (stream : Stream<_>) = stream.SnapshotImpl (cell1, cell2, cell3, cell4, (Func<_,_,_,_,_,_> f))

let snapshot5B behavior1 behavior2 behavior3 behavior4 behavior5 f stream =
    stream |> snapshot4B behavior1 behavior2 behavior3 behavior4 tuple5S |> snapshotB behavior5 (fun struct (a, b, c, d, e) f' -> f a b c d e f')

let snapshot5 cell1 cell2 cell3 cell4 cell5 f stream =
    stream |> snapshot5B (cell1 |> Cell.asBehavior) (cell2 |> Cell.asBehavior) (cell3 |> Cell.asBehavior) (cell4 |> Cell.asBehavior) (cell5 |> Cell.asBehavior) f

let snapshot6B behavior1 behavior2 behavior3 behavior4 behavior5 behavior6 f stream =
    stream |> snapshot4B behavior1 behavior2 behavior3 behavior4 tuple5S |> snapshot2B behavior5 behavior6 (fun struct (a, b, c, d, e) f' g -> f a b c d e f' g)

let snapshot6 cell1 cell2 cell3 cell4 cell5 cell6 f stream =
    stream |> snapshot6B (cell1 |> Cell.asBehavior) (cell2 |> Cell.asBehavior) (cell3 |> Cell.asBehavior) (cell4 |> Cell.asBehavior) (cell5 |> Cell.asBehavior) (cell6 |> Cell.asBehavior) f

let snapshot7B behavior1 behavior2 behavior3 behavior4 behavior5 behavior6 behavior7 f stream =
    stream |> snapshot4B behavior1 behavior2 behavior3 behavior4 tuple5S |> snapshot3B behavior5 behavior6 behavior7 (fun struct (a, b, c, d, e) f' g h -> f a b c d e f' g h)

let snapshot7 cell1 cell2 cell3 cell4 cell5 cell6 cell7 f stream =
    stream |> snapshot7B (cell1 |> Cell.asBehavior) (cell2 |> Cell.asBehavior) (cell3 |> Cell.asBehavior) (cell4 |> Cell.asBehavior) (cell5 |> Cell.asBehavior) (cell6 |> Cell.asBehavior) (cell7 |> Cell.asBehavior) f

let snapshot8B behavior1 behavior2 behavior3 behavior4 behavior5 behavior6 behavior7 behavior8 f stream =
    stream |> snapshot4B behavior1 behavior2 behavior3 behavior4 tuple5S |> snapshot4B behavior5 behavior6 behavior7 behavior8 (fun struct (a, b, c, d, e) f' g h i -> f a b c d e f' g h i)

let snapshot8 cell1 cell2 cell3 cell4 cell5 cell6 cell7 cell8 f stream =
    stream |> snapshot8B (cell1 |> Cell.asBehavior) (cell2 |> Cell.asBehavior) (cell3 |> Cell.asBehavior) (cell4 |> Cell.asBehavior) (cell5 |> Cell.asBehavior) (cell6 |> Cell.asBehavior) (cell7 |> Cell.asBehavior) (cell8 |> Cell.asBehavior) f

[<MethodImpl(MethodImplOptions.NoInlining)>]
let merge f ((stream : Stream<_>), stream2) = stream.MergeImpl (stream2, (Func<_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let orElse ((stream : Stream<_>), stream2) = stream.OrElseImpl stream2

[<MethodImpl(MethodImplOptions.NoInlining)>]
let filter predicate (stream : Stream<_>) = stream.FilterImpl (Func<_,_> predicate)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let filterOption (stream : Stream<_>) =
    StreamExtensionMethodsInternal.FilterMaybeImpl (stream, (Action<_,_> (fun o a -> o |> Option.iter a.Invoke)))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let gateB (behavior : Behavior<_>) (stream : Stream<_>) = stream.GateImpl behavior

[<MethodImpl(MethodImplOptions.NoInlining)>]
let gate (cell : Cell<_>) (stream : Stream<_>) = stream.GateImpl cell

[<MethodImpl(MethodImplOptions.NoInlining)>]
let collectLazy initialState (f : 'a -> 'TState -> struct ('b * 'TState)) (stream : Stream<_>) = stream.CollectLazyImpl (initialState, (Func<_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let collect initialState (f : 'a -> 'TState -> struct ('b * 'TState)) (stream : Stream<_>) = stream.CollectImpl (initialState, (Func<_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let calmWithCompare compare (stream : Stream<_>) = stream.CalmImpl (Func<_,_,_> compare)

[<MethodImpl(MethodImplOptions.NoInlining)>]
let calmWithEqualityComparer (equalityComparer : IEqualityComparer<_>) (stream : Stream<_>) = stream.CalmImpl (Func<_,_,_> (fun x y -> equalityComparer.Equals (x, y)))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let calm (stream : Stream<_>) = stream.CalmImpl (Func<_,_,_> (=))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let accumLazy initialState f (stream : Stream<_>) = stream.AccumLazyImpl (initialState, (Func<_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let accum initialState f (stream : Stream<_>) = stream.AccumImpl (initialState, (Func<_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let once (stream : Stream<_>) = stream.OnceImpl ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let mergeAll f (streams : seq<_>) = StreamExtensionMethodsInternal.MergeImpl (streams, (Func<_,_,_> f))

[<MethodImpl(MethodImplOptions.NoInlining)>]
let orElseAll (streams : seq<_>) = StreamExtensionMethodsInternal.OrElseImpl streams