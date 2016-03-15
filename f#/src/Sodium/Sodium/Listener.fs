namespace Sodium

open System
open System.Collections.Generic
open System.Linq
open Priority_Queue

type IListener =
    inherit IDisposable
    abstract member Unlisten : unit -> unit

module Listener =

    type private ActionListener(unlisten : unit -> unit) =
        interface IListener with
            member __.Unlisten () = unlisten()
        interface IDisposable with
            member __.Dispose () = unlisten()

    let fromAction a : IListener = upcast new ActionListener(a)
    let empty = fromAction(fun() -> ())

    let append (l1 : IListener) (l2 : IListener) : IListener = upcast new ActionListener(fun() ->
        l1.Unlisten()
        l2.Unlisten())