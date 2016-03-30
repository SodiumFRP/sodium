namespace Sodium

open System
open System.Collections.Generic
open System.Linq

type private 'T KeepListenersAliveImplementation() =
    let listeners = HashSet<IListener>()
    let childKeepListenersAliveList = List<IKeepListenersAlive>()

    interface IKeepListenersAlive with
        member __.KeepListenerAlive listener = listeners.Add(listener) |> ignore
        member __.StopKeepingListenerAlive listener = listeners.Remove(listener) |> ignore
        member __.Use childKeepListenersAlive = childKeepListenersAliveList.Add(childKeepListenersAlive)

type private 'T StreamListener(stream : 'T StreamImpl, action : Transaction -> 'T -> unit, target : 'T Target) = 
    let unlisten() =
        action.ToString () |> ignore
        stream.Node.Unlink target
    
    interface IListener with
        member __.Unlisten() = unlisten()
    
    interface IDisposable with
        member __.Dispose() = unlisten()

and internal 'T StreamImpl private (keepListenersAlive : IKeepListenersAlive, node : 'T Node, cleanup : IListener, firings : 'T seq) =
    let mutable cleanup = cleanup
    let firings = List<'T>(firings)

    internal new(keepListenersAlive : IKeepListenersAlive) = new StreamImpl<'T>(keepListenersAlive, Node<'T>(0L), Listener.empty, [])
    internal new() = new StreamImpl<'T>(KeepListenersAliveImplementation())
    
    member val internal KeepListenersAlive = keepListenersAlive

    static member CreateCoalesceHandler (f : 'T -> 'T -> 'T) (stream : 'T StreamImpl) = 
        let mutable accum = Option.None
        (fun (transaction : Transaction) a ->
            match accum with
            | None -> 
                accum <- Option.Some a
                transaction.Prioritized stream.Node (fun transaction -> 
                    match accum with
                    | None -> invalidOp "There is no value to send."
                    | Some ac -> 
                        stream.Send(transaction, ac)
                        accum <- Option.None)
                accum <- Option.Some a
            | Some ac -> accum <- Option.Some(f ac a))
    
    member internal __.Node : 'T Node = node
    
    member internal this.ListenInternal target (transaction : Transaction) action suppressEarlierFirings : IListener = 
        let changed, nodeTarget = node.Link action target
        if changed then transaction.SetNeedsRegenerating()
        let firings = List.ofSeq firings
        if not suppressEarlierFirings && firings.Any() then
            transaction.Prioritized target (fun t ->
                for a in firings do
                    Transaction.InCallback <- Transaction.InCallback + 1
                    try 
                        action t a
                    finally
                        Transaction.InCallback <- Transaction.InCallback - 1)
        upcast new StreamListener<'T>(this, action, nodeTarget)
    
    member internal this.ListenWithTransaction target action = 
        Transaction.Apply(fun t -> this.ListenInternal target t action false)

    member internal this.ListenWeak handler = this.ListenWithTransaction Node<'T>.Null (fun _ a -> handler a)
    
    member internal this.Listen handler = 
        let innerListener : IListener = this.ListenWeak handler
        let mutable listenerReference = Option.None
        
        let listener = 
            (Listener.fromAction (fun () -> 
                 innerListener.Unlisten()
                 lock keepListenersAlive (fun () -> 
                     match listenerReference with
                     | None -> ()
                     | Some l -> keepListenersAlive.StopKeepingListenerAlive(l) |> ignore)))
        listenerReference <- Option.Some listener
        lock keepListenersAlive (fun () -> keepListenersAlive.KeepListenerAlive(listener) |> ignore)
        listener
    
    member internal __.AddCleanup listener =
        Transaction.Run(fun () ->
            let cleanupNew = Listener.append cleanup listener
            new StreamImpl<'T>(keepListenersAlive, node, cleanupNew, firings))
    
    member internal this.UnsafeAddCleanup listener =
        cleanup <- Listener.append cleanup listener
        this
    
    member internal this.Coalesce (transaction : Transaction) (f : 'T -> 'T -> 'T) =
        let out = new StreamImpl<'T>(keepListenersAlive)
        let h = StreamImpl.CreateCoalesceHandler f out
        let listener = this.ListenInternal out.Node transaction h false
        out.UnsafeAddCleanup listener
    
    member internal this.LastFiringOnly(transaction : Transaction) = 
        this.Coalesce transaction (fun _ s -> s)
    
    member internal __.Send(transaction : Transaction, a) = 
        if not (firings.Any()) then transaction.Last firings.Clear
        firings.Add(a)
        let targets = HashSet<'T Target>(node.GetListeners())
        for target in targets do
            transaction.Prioritized target.Node (fun transaction -> 
                Transaction.InCallback <- Transaction.InCallback + 1
                try 
                    let foundAction, action = target.Action.TryGetTarget()
                    if foundAction then action transaction a
                    else node.RemoveListener target
                finally
                    Transaction.InCallback <- Transaction.InCallback - 1)
    
    interface IDisposable with
        member __.Dispose() = cleanup.Unlisten()

type 'T Stream internal(impl : 'T StreamImpl) =
    member val internal Impl = impl

    interface IDisposable with
        member __.Dispose() = (impl :> IDisposable).Dispose()

type internal 'T StreamSinkImpl(coalesce : 'T -> 'T -> 'T) as this = 
    inherit StreamImpl<'T>()
    let coalesce = StreamImpl.CreateCoalesceHandler coalesce this
    internal new() = 
        new StreamSinkImpl<'T>(fun _ _ -> 
        invalidOp 
            "Send was called more than once in a transaction, which isn't allowed.  To combine the streams, pass a coalescing function to the StreamSink constructor.")
    member __.Send a = 
        Transaction.Apply(fun transaction -> 
            if Transaction.InCallback > 0 then invalidOp "Send() may not be called inside a Sodium callback."
            coalesce transaction a)

type 'T StreamSink internal(impl : 'T StreamSinkImpl) =
    inherit Stream<'T>(impl)
    member val internal Impl = impl

    member __.Send a = impl.Send a

type internal 'T StreamLoopImpl() = 
    inherit StreamImpl<'T>()

    let isAssignedLock = obj()
    let mutable isAssigned = false
    do 
        if not (Transaction.HasCurrentTransaction()) then 
            invalidOp "StreamLoop and CellLoop must be used within an explicit transaction."

    member internal __.IsAssigned = lock isAssignedLock (fun () -> isAssigned)

    member internal this.Loop (stream : 'T StreamImpl) =
        lock isAssignedLock (fun () ->
            if isAssigned then invalidOp "StreamLoop was looped more than once."
            isAssigned <- true)
        Transaction.Run (fun () ->
            this.UnsafeAddCleanup (stream.ListenWithTransaction this.Node (fun t a -> this.Send(t, a)))
            stream.KeepListenersAlive.Use(this.KeepListenersAlive)) |> ignore
        
type 'T StreamLoop internal(impl : 'T StreamLoopImpl) =
    inherit Stream<'T>(impl)
    member val internal Impl = impl