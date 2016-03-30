namespace Sodium

open System
open System.Collections.Generic

type IListener =
    inherit IDisposable
    abstract member Unlisten : unit -> unit

type CompositeListener(listeners : IListener seq) =
    let listeners = List<_>(listeners)

    member __.Add l = listeners.Add(l)
    member __.AddRange l = listeners.AddRange(l)

    member __.Unlisten () = for l in listeners do l.Unlisten()

    interface IListener with
        member __.Unlisten () = __.Unlisten ()
    interface IDisposable with
        member __.Dispose () = __.Unlisten ()

module Listener =

    type private ActionListener(unlisten : unit -> unit) =
        interface IListener with
            member __.Unlisten () = unlisten ()
        interface IDisposable with
            member __.Dispose () = unlisten ()

    type private ImmutableCompositeListener(listeners : IListener seq) =
        let listeners = List.ofSeq listeners
        let unlisten () = for listener in listeners do listener.Unlisten ()
        interface IListener with
            member __.Unlisten () = unlisten ()
        interface IDisposable with
            member __.Dispose () = unlisten ()

    let fromAction a : IListener = upcast new ActionListener(a)
    let empty = fromAction(fun() -> ())

    let fromSeq listeners : IListener = upcast new ImmutableCompositeListener(listeners)

    let append (l1 : IListener) (l2 : IListener) : IListener = upcast new ActionListener(fun() ->
        l1.Unlisten()
        l2.Unlisten())

type internal IKeepListenersAlive =
    abstract member KeepListenerAlive : IListener -> unit
    abstract member StopKeepingListenerAlive : IListener -> unit
    abstract member Use : IKeepListenersAlive -> unit