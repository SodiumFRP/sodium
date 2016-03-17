namespace Sodium

open System
open System.Collections.Generic
open System.Linq

type private 'T StreamListener(stream : 'T Stream, action : Transaction -> 'T -> unit, target : 'T Target) = 
    let unlisten() =
        action.ToString () |> ignore
        stream.Node.Unlink target
    
    interface IListener with
        member __.Unlisten() = unlisten()
    
    interface IDisposable with
        member __.Dispose() = unlisten()

and 'T Stream private (node : 'T Node, cleanup : IListener, firings : 'T seq) =
    let mutable cleanup = cleanup
    let firings = List<'T>(firings)
    let keepListenersAlive = HashSet<IListener>()

    internal new() = new Stream<'T>(Node<'T>(0L), Listener.empty, [])
    
    static member CreateCoalesceHandler (f : 'T -> 'T -> 'T) (stream : 'T Stream) = 
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
                     | Some l -> keepListenersAlive.Remove(l) |> ignore)))
        listenerReference <- Option.Some listener
        lock keepListenersAlive (fun () -> keepListenersAlive.Add(listener) |> ignore)
        listener
    
    member internal __.AddCleanup listener =
        Transaction.Run(fun () ->
            let cleanupNew = Listener.append cleanup listener
            new Stream<'T>(node, cleanupNew, firings))
    
    member internal this.UnsafeAddCleanup listener =
        cleanup <- Listener.append cleanup listener
        this
    
    member internal this.Coalesce (transaction : Transaction) (f : 'T -> 'T -> 'T) =
        let out = new Stream<'T>()
        let h = Stream.CreateCoalesceHandler f out
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

type 'T StreamSink(coalesce : 'T -> 'T -> 'T) as this = 
    inherit Stream<'T>()
    let coalesce = Stream.CreateCoalesceHandler coalesce this
    internal new() = 
        new StreamSink<'T>(fun x y -> 
        invalidOp 
            "Send was called more than once in a transaction, which isn't allowed.  To combine the streams, pass a coalescing function to the StreamSink constructor.")
    member __.Send a = 
        Transaction.Apply(fun transaction -> 
            if Transaction.InCallback > 0 then invalidOp "Send() may not be called inside a Sodium callback."
            coalesce transaction a)

type private 'T StreamLoop() = 
    inherit Stream<'T>()

    let isAssignedLock = obj()
    let mutable isAssigned = false
    do 
        if not (Transaction.HasCurrentTransaction()) then 
            invalidOp "StreamLoop and CellLoop must be used within an explicit transaction."

    member internal __.IsAssigned = lock isAssignedLock (fun () -> isAssigned)

    member internal this.Loop (stream : 'T Stream) =
        lock isAssignedLock (fun () ->
            if isAssigned then invalidOp "StreamLoop was looped more than once."
            isAssigned <- true)
        Transaction.Run (fun () -> this.UnsafeAddCleanup (stream.ListenWithTransaction this.Node (fun t a -> this.Send(t, a)))) |> ignore