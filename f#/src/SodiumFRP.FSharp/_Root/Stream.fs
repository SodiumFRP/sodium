namespace rec SodiumFRP.FSharp

open System
open System.Collections.Generic

type internal WeakListener<'T> (node : Node<'T>, target) =
    interface IListenerWithWeakReference with
        member __.Unlisten () = node.Unlink target

type internal ListenerImplementation<'T> (stream : Stream<'T>, action : Transaction -> 'T -> unit, target : Target<'T>) =
    let weakListener = WeakListener (stream.Node, target)
    member internal __.Action = action // prevent GC
    member internal __.Stream = stream // prevent GC
    interface IWeakListener with
        member __.Unlisten () = Listener.unlistenWeak weakListener
        member __.GetListenerWithWeakReference () = upcast weakListener

type internal KeepListenersAlive () =
    let listeners = HashSet<IListener>()
    let childKeepListenersAliveList = List<IKeepListenersAlive>()
    interface IKeepListenersAlive with
        member __.KeepListenerAlive listener = listeners.Add listener |> ignore
        member __.StopKeepingListenerAlive listener = listeners.Remove listener |> ignore
        member __.Use childKeepListenersAlive = childKeepListenersAliveList.Add childKeepListenersAlive

type Stream<'T> =
    val private streamId : Guid
    val internal Node : Node<'T>
    val private attachedListeners : List<IListener>
    val private trackedListeners : StreamListeners
    val private firings : List<'T>
    val internal KeepListenersAlive : IKeepListenersAlive
    val private attachListenerLock : obj
    
    internal new (keepListenersAlive) =
        let streamId = Guid.NewGuid ()
        {
            streamId = streamId;
            Node = Node<_> ();
            attachedListeners = List ();
            trackedListeners = StreamListeners streamId
            firings = List ()
            KeepListenersAlive = keepListenersAlive;
            attachListenerLock = obj ()
        }
    
    internal new () = Stream (KeepListenersAlive())
    
    member internal this.ListenT target transaction action suppressEarlierFirings =
        let (changed, nodeTarget) = this.Node.Link transaction action target
        if changed then transaction.SetNeedsRegenerating ()
        let firings = Seq.toArray this.firings
        if not suppressEarlierFirings && firings.Length > 0 then
            transaction |> Transaction.prioritized target (fun transaction ->
                for a in firings do
                    TransactionInternal.inCallback <- TransactionInternal.inCallback + 1
                    try action transaction a
                    finally TransactionInternal.inCallback <- TransactionInternal.inCallback - 1)
        ListenerImplementation (this, action, nodeTarget) :> IWeakListener
    
    member internal this.Listen target action =
        Transaction.Apply (fun transaction _ -> this.ListenT target transaction action false) false
    
    member internal this.UnsafeAttachListener cleanup =
        this.attachedListeners.Add cleanup
        this.trackedListeners.AddListener <| cleanup.GetListenerWithWeakReference ()
        this
    
    member internal this.AttachListener listener =
        lock this.attachListenerLock (fun () -> this.UnsafeAttachListener listener)
    
    member internal this.Send (transaction : Transaction) a =
        if this.firings.Count < 1 then transaction.Last this.firings.Clear
        this.firings.Add a
        for target in this.Node.GetListenersCopy () do
            transaction |> Transaction.prioritized target.Node (fun transaction ->
                TransactionInternal.inCallback <- TransactionInternal.inCallback + 1
                try
                    let (found, action) = target.Action.TryGetTarget ()
                    if not found then this.Node.RemoveListener target
                    else if target.IsActivated then action transaction a
                finally TransactionInternal.inCallback <- TransactionInternal.inCallback - 1)
    
    member internal this.Coalesce transaction f =
        let out = Stream(this.KeepListenersAlive)
        let h = Stream.createCoalesceHandler f out
        let l = this.ListenT out.Node transaction h false 
        out.UnsafeAttachListener l
    
    static member createCoalesceHandler f (out : Stream<'T>) =
        let mutable accum = None
        
        let getAccum () =
           match accum with
               | Some v -> v
               | None -> invalidOp "Code should not be able to reach this point."
           
        fun transaction a ->
           match accum with
               | Some v -> accum <- Some <| f v a
               | None ->
                   accum <- Some a
                   transaction |> Transaction.prioritized
                       out.Node
                       (fun transaction ->
                           out.Send transaction (getAccum ())
                           accum <- None)
    
    override this.Finalize () = StreamListenerManager.remove this.streamId
