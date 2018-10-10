module SodiumFRP.FSharp.Behavior
open System
open System.Collections.Generic
open SodiumFRP.FSharp
open SodiumFRP.FSharp
open SodiumFRP.FSharp
open SodiumFRP.FSharp
open SodiumFRP.FSharp
open SodiumFRP.FSharp
open SodiumFRP.FSharp
open SodiumFRP.FSharp.Listener

type private FanOutKeepListenersAlive(keepListenersAliveList : IReadOnlyList<IKeepListenersAlive>) =
    interface IKeepListenersAlive with
        member __.KeepListenerAlive listener = for keepListenersAlive in keepListenersAliveList do keepListenersAlive.KeepListenerAlive listener
        member __.StopKeepingListenerAlive listener = for keepListenersAlive in keepListenersAliveList do keepListenersAlive.StopKeepingListenerAlive listener
        member __.Use childKeepListenersAlive = for keepListenersAlive in keepListenersAliveList do keepListenersAlive.Use childKeepListenersAlive

let constant value = Behavior(value)
let constantLazy value : Behavior<_> =
    upcast Transaction.Apply (fun transaction _ -> Stream.never () |> Stream.holdLazyInternal transaction value) false

let loop f =
    Transaction.Apply
        (fun transaction _ ->
            let l = BehaviorLoop ()
            let struct (s, r) = f l
            l.Loop transaction s
            struct (s, r))
        false

let loopWithNoCaptures f =
    let struct (l, _) = loop (fun s -> struct (f s, ()))
    l

let private snt (behavior : Behavior<_>) = behavior.SampleNoTransaction ()

let sample (behavior : Behavior<_>) = behavior.Sample ()
let internal sampleLazyInternal transaction (behavior : Behavior<_>) = behavior.SampleLazy transaction
let sampleLazy behavior = Transaction.Apply (fun transaction _ -> behavior |> sampleLazyInternal transaction) false

let internal updates (behavior : Behavior<_>) = behavior.Updates ()
let internal value transaction behavior =
    let updates = behavior |> updates
    let spark = Stream<unit>(updates |> Stream.keepListenersAlive)
    transaction |> Transaction.prioritized spark.Node (fun transaction -> spark.Send transaction ())
    let initial = spark |> Stream.snapshotAndTakeB behavior
    (initial, updates) |> Stream.mergeInternal transaction (fun left right -> right)

let apply f behavior : Behavior<_> =
    upcast Transaction.Apply
        (fun transaction _ ->
            let out = Stream(behavior |> updates |> Stream.keepListenersAlive)
            let outTarget = out.Node
            let inTarget = Node<_> ()
            let (changed, nodeTarget) = inTarget.Link transaction (fun _ _ -> ()) outTarget
            if changed then transaction.SetNeedsRegenerating ()
            let mutable fo = None
            let mutable ao = None
            let handler transaction f a = transaction |> Transaction.prioritized out.Node (fun transaction -> out.Send transaction <| f a)
            let handler2 transaction a f = handler transaction f a
            let listener1 =
                f
                    |> value transaction
                    |> Stream.listenT
                        inTarget
                        transaction
                        (fun transaction f ->
                            fo <- Some f
                            ao |> Option.iter (handler transaction f))
                        false
            let listener2 =
                behavior
                    |> value transaction
                    |> Stream.listenT
                        inTarget
                        transaction
                        (fun transaction a ->
                            ao <- Some a
                            fo |> Option.iter (handler2 transaction a))
                        false
            out
                |> Stream.lastFiringOnly transaction
                |> Stream.ual listener1
                |> Stream.ual listener2
                |> Stream.ual (Listener.fromNodeAndTarget inTarget nodeTarget)
                |> Stream.holdLazyInternal transaction (lazy (f |> snt) (behavior |> snt)))
        false

let map f behavior : Behavior<_> =
    upcast Transaction.Apply
        (fun transaction _ ->
            behavior
                |> updates
                |> Stream.map f
                |> Stream.holdLazyInternal transaction (behavior |> sampleLazyInternal transaction |> Lazy.map f))
        false

let lift2 f (behavior, behavior2) = behavior2 |> apply (behavior |> map f)
let lift3 f (behavior, behavior2, behavior3) = behavior3 |> apply (behavior2 |> apply (behavior |> map f))

let lift4 f (behavior, behavior2, behavior3, behavior4) =
    behavior4 |> apply (behavior3 |> apply (behavior2 |> apply (behavior |> map f)))

let lift5 f (behavior, behavior2, behavior3, behavior4, behavior5) =
    behavior5 |> apply (behavior4 |> apply (behavior3 |> apply (behavior2 |> apply (behavior |> map f))))

let lift6 f (behavior, behavior2, behavior3, behavior4, behavior5, behavior6) =
    behavior6 |> apply (behavior5 |> apply (behavior4 |> apply (behavior3 |> apply (behavior2 |> apply (behavior |> map f)))))

let lift7 f (behavior, behavior2, behavior3, behavior4, behavior5, behavior6, behavior7) =
    behavior7 |> apply (behavior6 |> apply (behavior5 |> apply (behavior4 |> apply (behavior3 |> apply (behavior2 |> apply (behavior |> map f))))))

let lift8 f (behavior, behavior2, behavior3, behavior4, behavior5, behavior6, behavior7, behavior8) =
    behavior8 |> apply (behavior7 |> apply (behavior6 |> apply (behavior5 |> apply (behavior4 |> apply (behavior3 |> apply (behavior2 |> apply (behavior |> map f)))))))

let private keepListenersAlive (behavior : Behavior<_>) = behavior.KeepListenersAlive

let internal liftAllInternal (f : IReadOnlyList<_> -> _) (behaviors : IReadOnlyCollection<_>) : Behavior<_> =
    upcast Transaction.Apply
        (fun transaction _ ->
            let out = Stream (FanOutKeepListenersAlive (behaviors |> Seq.map keepListenersAlive |> Array.ofSeq))
            let initialValue = lazy f (behaviors |> Seq.map snt |> Array.ofSeq)
            let listeners =
                behaviors
                    |> Seq.mapi
                        (fun i behavior ->
                            behavior
                                |> updates
                                |> Stream.listenT
                                    out.Node
                                    transaction
                                    (fun transaction v -> out.Send transaction <| (fun (vv : array<_>) -> vv.[i] <- v))
                                    false)
                    |> Array.ofSeq
            out
                |> Stream.coalesce transaction Func.combine
                |> Stream.map
                    (fun a ->
                        let values = behaviors |> Seq.map snt |> Array.ofSeq
                        a values
                        f values)
                |> Stream.ual (Listener.fromList listeners)
                |> Stream.holdLazyInternal transaction initialValue
        )
        false

let liftAll f (behaviors : seq<'a>) =
    liftAllInternal
        f
        (match behaviors with
            | :? IReadOnlyCollection<'a> as behaviors -> behaviors
            | _ -> Array.ofSeq behaviors :> IReadOnlyCollection<_>)

let switchB behavior : Behavior<_> =
    upcast Transaction.Apply
        (fun transaction _ ->
            let za = behavior |> sampleLazy |> Lazy.map sample
            let out = Stream (behavior |> keepListenersAlive)
            let currentListener = MutableListener ()
            let handler transaction behavior =
                currentListener |> MutableListener.unlisten
                currentListener
                    |> MutableListener.setListener
                        (behavior |> value transaction |> Stream.listenT out.Node transaction out.Send false)
            let listener = behavior |> value transaction |> Stream.listenT out.Node transaction handler false
            out |> Stream.ual listener |> Stream.ual currentListener |> Stream.holdLazyInternal transaction za
        )
        false

let private beh (cell : Cell<_>) = cell.Behavior

let switchC behavior = Cell(behavior |> map beh |> switchB)

let switchS behavior =
    Transaction.Apply
        (fun transaction _ ->
            let out = Stream (behavior |> keepListenersAlive)
            let currentListener = MutableListener ()
            let handlerInitial transaction stream =
                currentListener |> MutableListener.unlisten
                currentListener
                    |> MutableListener.setListener (stream |> Stream.listenT out.Node transaction out.Send false)
            let handler (transaction : Transaction) stream =
                transaction.Last (fun () ->
                    currentListener |> MutableListener.unlisten
                    currentListener
                        |> MutableListener.setListener (stream |> Stream.listenT out.Node transaction out.Send true))
            transaction
                |> Transaction.prioritized
                    (Node<_> ()) (fun transaction -> handlerInitial transaction (behavior |> snt))
            let listener = behavior |> updates |> Stream.listenT (Node<_> ()) transaction handler false
            out |> Stream.ual listener |> Stream.ual currentListener
        )
        false