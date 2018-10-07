namespace SodiumFRP.FSharp

open System
open System.Collections.Generic

type IListenerWithWeakReference =
    abstract member Unlisten : unit -> unit

type IListener =
    abstract member Unlisten : unit -> unit
    abstract member GetListenerWithWeakReference : unit -> IListenerWithWeakReference

type IStrongListener =
    inherit IListener
    inherit IDisposable

type IWeakListener =
    inherit IListener

module Listener =

    let unlistenWeak (listener : IListenerWithWeakReference) = listener.Unlisten ()
    let unlisten (listener : IListener) = listener.Unlisten ()
    let getListenerWithWeakReference (listener : IListener) = listener.GetListenerWithWeakReference ()

    type private ActionListener(unlisten : unit -> unit) =
        interface IListener with
            member __.Unlisten () = unlisten ()
            member this.GetListenerWithWeakReference () = upcast this
        interface IListenerWithWeakReference with
            member __.Unlisten () = unlisten ()
        interface IDisposable with
            member __.Dispose () = unlisten ()

    type private CompositeListener<'T when 'T :> IListener>(listeners : IReadOnlyList<'T>) =
        let listenersWeak = lazy (Seq.map getListenerWithWeakReference listeners |> Array.ofSeq)
        interface IListener with
            member __.Unlisten () = for listener in listeners do listener.Unlisten ()
            member this.GetListenerWithWeakReference () = upcast this
        interface IListenerWithWeakReference with
            member __.Unlisten () = for listener in listenersWeak.Value do listener.Unlisten ()
    
    type private CompositeWeakListener(listeners) =
        inherit CompositeListener<IWeakListener>(listeners)
        interface IWeakListener with
            member this.Unlisten () = unlisten this
    
    type private CompositeStrongListener(listeners) =
        inherit CompositeListener<IStrongListener>(listeners)
        interface IStrongListener with
            member this.Unlisten () = unlisten this
            member this.Dispose () = unlisten this

    let internal fromAction a : IListener = upcast new ActionListener(a)
    let internal fromNodeAndTarget (node : Node<_>) target = fromAction (fun () -> node.Unlink target)
    let empty = fromAction(fun() -> ())

    let fromList listeners : IListener = upcast new CompositeListener<#IListener>(listeners)
    let fromWeakList listeners : IWeakListener = upcast new CompositeWeakListener(listeners)
    let fromStrongList listeners : IStrongListener = upcast new CompositeStrongListener(listeners)
    let fromSeq listeners = List.ofSeq listeners |> fromList

    let append (l1 : IListener) (l2 : IListener) : IListener = upcast new ActionListener(fun() ->
        l1.Unlisten()
        l2.Unlisten())

type internal IKeepListenersAlive =
    abstract member KeepListenerAlive : IListener -> unit
    abstract member StopKeepingListenerAlive : IListener -> unit
    abstract member Use : IKeepListenersAlive -> unit
