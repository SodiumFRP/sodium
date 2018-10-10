module SodiumFRP.FSharp.Stream

open SodiumFRP.FSharp
open System.Collections.Generic
open System.Threading.Tasks

type private StrongListener (unlisten, listener) =
    interface IStrongListener with
        member __.Unlisten () = unlisten ()
        member __.GetListenerWithWeakReference () = Listener.getListenerWithWeakReference listener
        member __.Dispose () = unlisten ()

let never<'a> () = Stream<'a> ()

let loop f =
    Transaction.Apply
        (fun transaction _ ->
            let l = StreamLoop ()
            let struct (s, r) = f l
            l.Loop transaction s
            struct (s, r))
        false

let loopWithNoCaptures f =
    let struct (l, _) = loop (fun s -> struct  (f s, ()))
    l

let internal listenT target transaction action suppressEarlierFirings (stream : Stream<_>) =
    stream.ListenT target transaction action suppressEarlierFirings

let internal listenN target action (stream : Stream<_>) = stream.Listen target action

let internal keepListenersAlive (stream : Stream<_>) = stream.KeepListenersAlive

let listenWeak handler (stream : Stream<_>) = stream |> listenN Node.Null (fun transaction a -> handler a)

let listen handler stream =
    let innerListener = stream |> listenWeak handler
    let mutable listenerOption = None
    let keepListenerAlive listener =
        lock stream.KeepListenersAlive (fun () -> stream.KeepListenersAlive.KeepListenerAlive listener)
    let stopKeepingListenerAlive listener =
        lock stream.KeepListenersAlive (fun () -> stream.KeepListenersAlive.StopKeepingListenerAlive listener)
    let unlisten () =
        Listener.unlisten innerListener
        listenerOption |> Option.iter stopKeepingListenerAlive
    let listener = new StrongListener (unlisten, innerListener)
    listenerOption <- Some listener
    keepListenerAlive listener
    listener :> IStrongListener

let attachListener listener (stream : Stream<_>) = stream.AttachListener listener

let listenOnce handler stream =
    let mutable listenerOption = None
    let mutable unlistenEarly = false
    let listener =
        stream |> listen
            (fun a ->
                match listenerOption with
                    | None -> unlistenEarly <- true
                    | Some listener ->
                        listener |> Listener.unlisten
                        listenerOption <- None
                handler a)
    listenerOption <- Some listener
    if unlistenEarly then
        listener |> Listener.unlisten
        listenerOption <- None
    listener

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

let internal ual listener (stream : Stream<_>) = stream.UnsafeAttachListener listener

let map f stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener = stream |> listenN out.Node (fun transaction a -> out.Send transaction <| f a)
    out |> ual listener

let mapTo value stream = stream |> map (fun _ -> value)
let internal holdInternal initialValue stream = Behavior (stream, initialValue)
let hold initialValue stream = Cell (stream |> (holdInternal initialValue))
let internal holdLazyInternal transaction initialValue stream = LazyBehavior (transaction, stream, initialValue)
let holdLazy initialValue stream = Transaction.Apply (fun transaction _ -> Cell (stream |> (holdLazyInternal transaction initialValue))) false

let private snt (behavior : Behavior<_>) = behavior.SampleNoTransaction ()
let private beh (cell : Cell<_>) = cell.Behavior

let snapshotB behavior f stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener =
        stream |> listenN out.Node (fun transaction a -> out.Send transaction <| f a (behavior |> snt))
    out |> ual listener

let snapshot cell f stream = stream |> snapshotB (cell |> beh) f
let snapshotAndTakeB behavior stream = stream |> snapshotB behavior (fun _ a -> a)
let snapshotAndTake cell stream = stream |> snapshotAndTakeB (cell |> beh)

let snapshot2B behavior1 behavior2 f stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener =
        stream |> listenN out.Node (fun transaction a -> out.Send transaction <| f a (behavior1 |> snt) (behavior2 |> snt))
    out |> ual listener

let snapshot2 cell1 cell2 f stream = stream |> snapshot2B (cell1 |> beh) (cell2 |> beh) f

let snapshot3B behavior1 behavior2 behavior3 f stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener =
        stream |> listenN out.Node (fun transaction a -> out.Send transaction <| f a (behavior1 |> snt) (behavior2 |> snt) (behavior3 |> snt))
    out |> ual listener

let snapshot3 cell1 cell2 cell3 f stream = stream |> snapshot3B (cell1 |> beh) (cell2 |> beh) (cell3 |> beh) f

let snapshot4B behavior1 behavior2 behavior3 behavior4 f stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener =
        stream |> listenN out.Node (fun transaction a -> out.Send transaction <| f a (behavior1 |> snt) (behavior2 |> snt) (behavior3 |> snt) (behavior4 |> snt))
    out |> ual listener

let snapshot4 cell1 cell2 cell3 cell4 f stream =
    stream |> snapshot4B (cell1 |> beh) (cell2 |> beh) (cell3 |> beh) (cell4 |> beh) f

let snapshot5B behavior1 behavior2 behavior3 behavior4 behavior5 f stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener =
        stream |> listenN out.Node (fun transaction a -> out.Send transaction <| f a (behavior1 |> snt) (behavior2 |> snt) (behavior3 |> snt) (behavior4 |> snt) (behavior5 |> snt))
    out |> ual listener

let snapshot5 cell1 cell2 cell3 cell4 cell5 f stream =
    stream |> snapshot5B (cell1 |> beh) (cell2 |> beh) (cell3 |> beh) (cell4 |> beh) (cell5 |> beh) f

let snapshot6B behavior1 behavior2 behavior3 behavior4 behavior5 behavior6 f stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener =
        stream |> listenN out.Node (fun transaction a -> out.Send transaction <| f a (behavior1 |> snt) (behavior2 |> snt) (behavior3 |> snt) (behavior4 |> snt) (behavior5 |> snt) (behavior6 |> snt))
    out |> ual listener

let snapshot6 cell1 cell2 cell3 cell4 cell5 cell6 f stream =
    stream |> snapshot6B (cell1 |> beh) (cell2 |> beh) (cell3 |> beh) (cell4 |> beh) (cell5 |> beh) (cell6 |> beh) f

let snapshot7B behavior1 behavior2 behavior3 behavior4 behavior5 behavior6 behavior7 f stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener =
        stream |> listenN out.Node (fun transaction a -> out.Send transaction <| f a (behavior1 |> snt) (behavior2 |> snt) (behavior3 |> snt) (behavior4 |> snt) (behavior5 |> snt) (behavior6 |> snt) (behavior7 |> snt))
    out |> ual listener

let snapshot7 cell1 cell2 cell3 cell4 cell5 cell6 cell7 f stream =
    stream |> snapshot7B (cell1 |> beh) (cell2 |> beh) (cell3 |> beh) (cell4 |> beh) (cell5 |> beh) (cell6 |> beh) (cell7 |> beh) f

let snapshot8B behavior1 behavior2 behavior3 behavior4 behavior5 behavior6 behavior7 behavior8 f stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener =
        stream |> listenN out.Node (fun transaction a -> out.Send transaction <| f a (behavior1 |> snt) (behavior2 |> snt) (behavior3 |> snt) (behavior4 |> snt) (behavior5 |> snt) (behavior6 |> snt) (behavior7 |> snt) (behavior8 |> snt))
    out |> ual listener

let snapshot8 cell1 cell2 cell3 cell4 cell5 cell6 cell7 cell8 f stream =
    stream |> snapshot8B (cell1 |> beh) (cell2 |> beh) (cell3 |> beh) (cell4 |> beh) (cell5 |> beh) (cell6 |> beh) (cell7 |> beh) (cell8 |> beh) f

let internal coalesce transaction f (stream : Stream<_>) = stream.Coalesce transaction f

let private mergePrivate transaction (stream, stream2) =
    let out = Stream (stream |> keepListenersAlive)
    let left = Node<_>()
    let right = out.Node
    let (changed, nodeTarget) = left.Link transaction (fun _ _ -> ()) right
    if changed then transaction.SetNeedsRegenerating ()
    let handler = out.Send
    let listener1 = stream |> listenN left handler
    let listener2 = stream2 |> listenN right handler
    out |> ual listener1 |> ual listener2 |> ual (Listener.fromNodeAndTarget left nodeTarget)

let internal mergeInternal transaction f (stream, stream2) =
    (stream, stream2) |> mergePrivate transaction |> coalesce transaction f

let internal lastFiringOnly transaction stream = stream |> coalesce transaction (fun first second -> second)

let merge f (stream, stream2) =
    Transaction.Apply (fun transaction _ -> (stream, stream2) |> mergeInternal transaction f) false

let orElse (stream, stream2) = (stream, stream2) |> merge (fun left right -> left)

let filter predicate stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener = stream |> listenN out.Node (fun transaction a -> if predicate a then out.Send transaction a)
    out |> ual listener

let filterOption stream =
    let out = Stream (stream |> keepListenersAlive)
    let listener = stream |> listenN out.Node (fun transaction a -> a |> Option.iter (fun v -> out.Send transaction v))
    out |> ual listener

let gateB behavior stream = stream |> snapshotB behavior (fun a v -> if v then Some a else None) |> filterOption
let gate cell stream = stream |> gateB (cell |> beh)

let collectLazy initialState (f : 'a -> 'TState -> struct ('b * 'TState)) stream =
    loop (fun stateLoop ->
        let s = stateLoop |> holdLazy initialState
        let both = stream |> snapshot s f
        struct (both |> map (fun x -> sndS x), both |> map (fun x -> fstS x))) |> sndS
    
let collect initialState (f : 'a -> 'TState -> struct ('b * 'TState)) stream = collectLazy (lazy initialState) f stream

let internal calmInternal initialValue compare stream =
    stream |> collectLazy
        initialValue
        (fun a lastA ->
            if (match lastA with | None -> false | Some v -> compare v a)
            then struct (None, lastA)
            else
                let oa = Some a
                struct (oa, oa)) |> filterOption

let calmWithCompare compare stream = stream |> calmInternal (lazy None) compare

let calmWithEqualityComparer (equalityComparer : IEqualityComparer<_>) stream =
    stream |> calmWithCompare (fun x y -> equalityComparer.Equals (x, y))

let calm stream = stream |> calmWithCompare (=)

let accumLazy initialState f stream =
    let result = loopWithNoCaptures (fun stateLoop ->
        let s = stateLoop |> holdLazy initialState
        stream |> snapshot s f)
    result |> holdLazy initialState

let accum initialState f stream = accumLazy (lazy initialState) f stream

let once stream =
    let out = Stream(stream |> keepListenersAlive)
    let mutable listenerOption = None
    let mutable unlistenEarly = false
    let listener = stream |> listenN out.Node (fun transaction a ->
        match listenerOption with
        | None -> unlistenEarly <- true
        | Some listener ->
            out.Send transaction a
            Listener.unlisten listener
            listenerOption <- None)
    listenerOption <- Some listener
    if unlistenEarly then
        listener |> Listener.unlisten
        listenerOption <- None
    out |> ual listener

let mergeAll f streams =
    
    let rec mergeAll f start ``end`` (streams : list<#Stream<_>>) =
        let n = ``end`` - start
        if n = 0 then never ()
        elif n = 1 then upcast streams.[start]
        elif n = 2 then (streams.[start], streams.[start + 1]) |> merge f
        else
            let mid = (start + ``end``) / 2
            (streams |> mergeAll f start mid, streams |> mergeAll f mid ``end``) |> merge f
    
    let s = List.ofSeq streams
    s |> mergeAll f 0 s.Length

let orElseAll streams = streams |> mergeAll (fun left right -> left)